---
layout: post
category: programming
---

<!--start-excerpt-->My goal here is to regrid geographical data onto another lat-lon grid.  Any library or module needed will be unloaded after the process.  I have tested PyFerret and the Python spherical harmonic module (hereafter PySpHarm) and presented here how to implement PyFerret for regridding purpose. <!--end-excerpt-->  In most cases, both PyFerret and the Python and PySpHarm do a pretty good job regridding geographical 2D data and preserve area averages.  Here are examples going from higher resolutions to lower resolutions and vice versa.

<div class="row">
  <div class="col-sm-6 col-md-6">
    <div class="thumbnail">
		<a href="/assets/images/regridding_py_lower_crop.png">
			<img data-src="holder.js/300x200" src="/assets/images/regridding_py_lower_crop.png"></a>
      <div class="caption">
        From higher resolutions (0.66 degree longitude x 0.5 degree latitude) to lower resolutions (2.66 degree longitude x 2 degree latitude)
      </div>
    </div>
  </div>
  <div class="col-sm-6 col-md-6">
    <div class="thumbnail">
      <a href="/assets/images/regridding_py_higher_crop.png">
		  <img data-src="holder.js/300x200" src="/assets/images/regridding_py_higher_crop.png"></a>
      <div class="caption">
        From lower resolutions (2.66 degree longitude x 2 degree latitude) to higher resolutions (0.33 degree longitude x 0.25 degree latitude)
      </div>
    </div>
  </div>
</div>

Pros and Cons
---

**1. Installation - PySpHarm wins**

For PyFerret, the major drawback is the fact that it is not quite trivial to install while for PySpHarm it is simple (see its [documentation](http://pyspharm.googlecode.com/svn/trunk/html/index.html)).

**2. Annoying ripple patterns for PySpHarm  - PyFerret wins**

[Gibbs fringes](http://en.wikipedia.org/wiki/Gibbs_phenomenon) are inevitable for spectral harmonics tranforms although they can be minimised by applying filters (See [Navarra 1994](http://journals.ametsoc.org/doi/abs/10.1175/1520-0442%281994%29007%3C1169%3AROTGOI%3E2.0.CO%3B2) and references therein).  In contrast, PyFerret provides various regridding methods: linear interpolation, patch recovery by taking least squeares fit of the surrounding surface patches and conservative methods.  These methods do not generate Gibbs ripples.

**3. Speed - PyFerret wins**

The computational complexity of the spherical harmonics transform is O(N^3) for cut-off frequency N.  Some algorithms allow for a running time of O(N^2logN).  I am not sure what the actual algorithm is used by PySpHarm but in my experience it is far slower than PyFerret in most cases.  The performance difference is more obvious when only a region of the globe needs regridding.


Implementing PyFerret for regridding
---

**You may download the script [here](/assets/scripts/pyferret_regrid.py.txt)**.  Below I present slightly more details for thoughts.

**0. Preconditions**

- You have installed the PyFerret module and are able to <code class="python">import pyferret</code>.  If not, you may refer to my previous [post on installing and building PyFerret]({% post_url 2014-04-13-Installing-and-Building-PyFerret %})
- You should have NumPy installed as well.

**1. Main action**

Using PyFerret still requires some knowledge of Ferret commands.  Basically my python function here is to stitch the Ferret commands together for regridding.

In Ferret, to regrid a variable <code>SOURCE</code> to the lat-lon grid of <code>DEST</code> you would write:
<code>LET RESULT = SOURCE\[GXY=DEST\]</code>

This is included in the function <code>regrid_once_primitive</code>.

What *have* been taken care of here:
- The data returned from Ferret may (usually) have a different axis order than the input's.  Reorder the dimensions
- Instead of specifying the axis type (T/X/Y/Z) by the users, units of the dimensions (usually available) are used for assigning the axis types.  If the unit is not recognised, a normal axis is assigned
- Tranformation method can be specified

What *have not* been taken care of:
- Regridding on the time axis is not implemented yet
-
For some reasons when pyferret is stopped, there is still memory of the previous data grid being kept so that the next time pyferret is started, dimensions of the same names used before (e.g. latitude, longitude) cannot be recognised.  This might be associated with one of the [known issues](http://ferret.pmel.noaa.gov/Ferret/documentation/pyferret/known-issues) of PyFerret.  To work around this, I use the python multiprocessing library to run the PyFerret on a separate process so that each time when the function is finished the process is killed and the memory associated is freed (I think so?).

{% highlight python %}
# This decorator is a work around needed for
# calling the regrid function multiple times
def run_worker(f):
    import multiprocessing
    @wraps(f)
    def run_func(*args,**kwargs):
        P = multiprocessing.Pool(1)
        result = P.apply(f,args,kwargs)
        P.close()
        P.terminate()
        P.join()
        return result
    return run_func

# This is the regrid function to be called
# And it will be run as a stand-alone process that terminates after each call
regrid = run_worker(regrid_once_primitive)

{% endhighlight %}
**2. Interfaces for adding and extracting data to/from PyFerret**

PyFerret.getdata and PyFerret.putdata get/read python dictionary objects.  I wrote my own wrappers, <code>__Num2Fer__</code> and <code>__Fer2Num__</code> for converting between my input and what PyFerret asks for.  For regridding purposes, you don't need the much information needed by PyFerret, I can fill in dummy values for fields such as "title","dimension names","data_units".

Consequently the regridding function only requests a data array, a list of coordinates, and a list of dimensions units for the input data, and the latter two for the output grid.

I also included a function <code>__assignCAxis__</code> that guesses the dimension type (T/X/Y/Z) from the dimension units.  This function has been very useful even in other situations.  I use it **a lot**.

{% highlight python %}
def _assignCAxis_(dimunit):
    ''' 
    Assign cartesian_axis (T/Z/Y/X) to the axis with identifiable axis units.
    Axes without identifiable units will be set to None
    Input: unit - a string
    '''
    assert type(dimunit) is str
    dimunit = dimunit.split()[0]
    conventions = {'T': ['second','seconds','sec','minute','minutes','min',
                        'hour','hours','h','day','days','d',
                        'month','months','mon','year','years','y'],
                    'Z': ['bar','millibar','decibar','atm','atmosphere','pascal','Pa','hpa',
                        'meter','m','kilometer','km','density'],
                    'Y': ['degrees_north','degree_north','degree_n','degrees_n','degreen','degreesn'],
                    'X': ['degrees_east','degree_east','degree_e','degrees_e','degreee','degreese']}
    invaxunits = { unit.lower():ax for ax,units in conventions.items() for unit in units }
    return invaxunits.get(dimunit.lower(),None)

{% endhighlight %}



**3. Run**

{% highlight python %}
if __name__ == '__main__':
    import numpy
    var={}
    var['data'] = numpy.arange(400.).reshape(20,20)
    var['coords'] = [ numpy.linspace(-10.,10.,20),
                      numpy.linspace(100.,160.,20)]
    var['dimunits'] = ['degrees_N','degrees_E']
    ref_var={}
    ref_var['coords'] = [ numpy.linspace(-10.,10.,10),
                      numpy.linspace(100.,160.,10)]
    ref_var['dimunits'] = ['degrees_N','degrees_E']
    result = regrid(var,ref_var,'XY')	
{% endhighlight %}


Results:

<div class="row">
	<div class="col-xs-12 col-md-8">
		<div class="thumbnail">
			<img data-src="holder.js/300x200" src="/assets/images/pyferret_regrid_example.png">
				<div class="caption">
					Regridding example.  Succeeded!
				</div>
	    </div>
	</div>
</div>

