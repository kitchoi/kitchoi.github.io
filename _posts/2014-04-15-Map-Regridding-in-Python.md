---
layout: post
category: programming
---

<!--start-excerpt-->In most cases, both PyFerret and the Python spherical harmonic module (hereafter PySpHarm) do a pretty good job regridding geographical 2D data and preserve area averages.  Here are examples going from higher resolutions to lower resolutions and vice versa. <!--end-excerpt-->

<div class="row">
  <div class="col-sm-6 col-md-4">
    <div class="thumbnail">
      <img data-src="holder.js/300x200" src="/assets/images/regridding_py_lower_crop.png">
      <div class="caption">
        From higher resolutions (0.66 degree longitude x 0.5 degree latitude) to lower resolutions (2.66 degree longitude x 2 degree latitude)
      </div>
    </div>
  </div>
  <div class="col-sm-6 col-md-4">
    <div class="thumbnail">
      <img data-src="holder.js/300x200" src="/assets/images/regridding_py_higher_crop.png">
      <div class="caption">
        From lower resolutions (2.66 degree longitude x 2 degree latitude) to higher resolutions (0.33 degree longitude x 0.25 degree latitude)
      </div>
    </div>
  </div>
</div>

Pros and Cons
---

**1. Installation - PySpHarm wins**

For PyFerret, the major drawback is the fact that it is not quite trivial to get it installed while for PySpHarm it is simple (See [documentation](http://pyspharm.googlecode.com/svn/trunk/html/index.html))

**2. Gibbs fringes from low to high resolution - PyFerret wins**

Gibbs fringes are unavoidable for spectral grids yet the impact can be minimised (See [Navarra 1994](http://journals.ametsoc.org/doi/abs/10.1175/1520-0442%281994%29007%3C1169%3AROTGOI%3E2.0.CO%3B2) and references therein)

**3. Speed - PyFerret wins**

The computational complexity of the spherical harmonics transform is O(N^3) for cut-off frequency N.  Some algorithms allow for a running time of O(N^2logN).  I am not sure what the actual algorithm is used by PySpHarm but in my experience it is far slower than PyFerret in most cases.  The performance difference is more obvious when only a region of the globe needs regridding.
(Note: Various regridding methods can be chosen in PyFerret: linear interpolation, patch recovery by taking least squeares fit of the surrounding surface patches and conservative methods.)


Implementing PyFerret for regridding
---

**1. Preparing input for pyferret**

{% highlight python %}
def Var2Fer(var_data,varname,missing_value,data_units,
	cartesian_axes,dimnames,dimunits,coords):
    ''' Create a dictionary that resemble the Ferret
	data variable structure to be passed to pyferret.putdata
    Input:
    var_data       - numpy.ndarray
    varname        - string
    missing_value  - float
    data_units     - string
    cartesian_axes - a list of characters that specifies
                     the cartesian axes (e.g. ['T','Y','X'])
    dimnames       - a list of strings for the dimension names
                     (e.g. ['time','lat','lon'])
    dimunits       - a list of strings for the dimension units
                     (e.g. ['months','degrees_N','degrees_E'])
    coords         - a list of numpy.ndarray
    Length of cartesian_axes, dimnames, dimunits and coords need
    to agree with the number of dimensions of var_data
    '''
    import numpy
    import pyferret
    if len(cartesian_axes) != var_data.ndim:
        raise Exception("Number of cartesian_axes does not match var_data.ndim")
	if len(dimnames) != var_data.ndim:
        raise Exception("Number of dimnames does not match var_data.ndim")
    if len(dimunits) != var_data.ndim:
        raise Exception("Number of dimunits does not match var_data.ndim")
    if len(coords) != var_data.ndim:
        raise Exception("Number of coords does not match var_data.ndim")
    fer_var = {}
    # Define the variable
    fer_var['data'] = var_data.copy()
    # Variable name
    fer_var['name'] = varname
    # Dataset
    fer_var['dset'] = None
    # Title = variable name
    fer_var['title'] = fer_var['name']
    # Set missing value
    fer_var['missing_value'] = missing_value
    # Set data unit
    fer_var['data_unit'] = data_units
    # Determine the axis type
    cax2ax_type = {'X': pyferret.AXISTYPE_LONGITUDE,
                   'Y': pyferret.AXISTYPE_LATITUDE,
                   'Z': pyferret.AXISTYPE_LEVEL,
                   'T': pyferret.AXISTYPE_CUSTOM }
    fer_var['axis_types'] = [ cax2ax_type[cax] 
                              if cax in cax2ax_type.keys() 
                              else pyferret.AXISTYPE_NORMAL
                              for cax in cartesian_axes ]
    fer_var['axis_names'] = dimnames
    fer_var['axis_units'] = dimunits
    fer_var['axis_coords'] = coords
    # This will be used as the second argument to pyferret.putdata
    axis_pos_dict = {'X': pyferret.X_AXIS,
                     'Y': pyferret.Y_AXIS,
                     'Z': pyferret.Z_AXIS,
                     'T': pyferret.T_AXIS}
    # Force axis position
    fer_var['axis_pos'] = [ axis_pos_dict[cax]
                            if cax in axis_pos_dict.keys()
                            else cartesian_axes.index(cax)
                            for cax in cartesian_axes ]
    return fer_var

{% endhighlight %}

My function utilises util.nc.Variable which is a class I wrote for managing geographical data.
