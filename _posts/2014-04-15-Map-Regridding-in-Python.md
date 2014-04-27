---
layout: post
category: programming
---

<!--start-excerpt-->My goal here is to regrid geographical data onto another lat-lon grid provided by another data variable.  Any library or module needed will be unloaded after the process.  I have tested PyFerret and the Python spherical harmonic module (hereafter PySpHarm) and presented here how to implement PyFerret for regridding purpose. <!--end-excerpt-->  In most cases, both PyFerret and the Python and PySpHarm do a pretty good job regridding geographical 2D data and preserve area averages.  Here are examples going from higher resolutions to lower resolutions and vice versa.

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

For PyFerret, the major drawback is the fact that it is not quite trivial to install while for PySpHarm it is simple (see its [documentation](http://pyspharm.googlecode.com/svn/trunk/html/index.html)).

**2. Annoying ripple patterns for PySpHarm  - PyFerret wins**

[Gibbs fringes](http://en.wikipedia.org/wiki/Gibbs_phenomenon) are inevitable for spectral harmonics tranforms although they can be minimised by applying filters (See [Navarra 1994](http://journals.ametsoc.org/doi/abs/10.1175/1520-0442%281994%29007%3C1169%3AROTGOI%3E2.0.CO%3B2) and references therein).  In contrast, PyFerret provides various regridding methods: linear interpolation, patch recovery by taking least squeares fit of the surrounding surface patches and conservative methods.  These methods do not generate Gibbs ripples.

**3. Speed - PyFerret wins**

The computational complexity of the spherical harmonics transform is O(N^3) for cut-off frequency N.  Some algorithms allow for a running time of O(N^2logN).  I am not sure what the actual algorithm is used by PySpHarm but in my experience it is far slower than PyFerret in most cases.  The performance difference is more obvious when only a region of the globe needs regridding.


Implementing PyFerret for regridding
---

**0. Preconditions**

Supposedly you have PyFerret module installed on your machine such that you can import pyferret from Python.  If you have not done so, you may want to refer to my previous [post on installing and building PyFerret]({% post_url 2014-04-13-Installing-and-Building-PyFerret %}).  And of course you should have NumPy installed as well.

**1. Main action**

Using PyFerret still requires some knowledge of Ferret commands.  Basically my python function here is to stitch the Ferret commands together for regridding.

In Ferret, to regrid a variable <code>SOURCE</code> to the lat-lon grid of <code>DEST</code> you would write:
<code>LET RESULT = SOURCE\[GXY=DEST\]</code>

This is included in the function <code>regrid_once_primitive</code> below.

{% highlightscroll python %}
def regrid_once_primitive(var,ref_var,axis,
                          verbose=False,prerun=None,transform='@ave'):
    ''' Now only deal with regridding without the time axis
    Input:
    var       - a dictionary contains arguments for Num2Fer
                Requires: data,coords,dimunits
    ref_var   - a dictionary contains arguments for Num2Fer. 
                This supplies the grid for regridding
                Requires: coords,dimunits
    axis      - the axis for regridding, currently accepts 'X' or 'Y' or 'XY' only 
                (i.e. 2D regridding)
    verbose   - whether to print progress (default: False)
    prerun    - a list of strings as commands to be run at the start (default: None)
    transform - @ave (Conserve area average),@lin (Linear interpolation),...see Ferret doc
    Return:
    a dictionary 
    '''
    import pyferret
    import numpy
    import util.nc as _NC
    pyferret.start(quiet=True,journal=verbose,
                   verify=False,server=True)
    # commands to run before regridding
    if prerun is not None:
        if type(prerun) is str:
            pyferret.run(prerun)
        elif type(prerun) is list:
            for s in prerun:
                if type(s) is str:
                    pyferret.run(prerun)
                else:
                    raise Exception("prerun has to be either a string or a list of string")
        else:
            raise Exception("prerun has to be either a string or a list of string")
    
    axis = axis.upper()
    # Make sure axis is a string denoting X or Y axis
    if axis not in ['X','Y','XY','YX']:
        raise Exception("Currently axis can only be X/Y/XY")
    
    # Construct the source data read by pyferret.putdata
    source_fer = Num2Fer(varname="source",**var)
    
    # Fill in unnecessary input for Ferret
    if not ref_var.has_key('data'):
        ref_var['data'] = numpy.zeros((1,)*len(ref_var['coords']))
    
    # Construct the destination data read by pyferret.putdata
    dest_fer = Num2Fer(varname="dest",**ref_var)
    
    if verbose:
        print source_fer
        print dest_fer
    pyferret.putdata(source_fer,axis_pos=source_fer['axis_pos'])
    if verbose: 
        print "Put source variable"
        pyferret.run('show grid source')
    pyferret.putdata(dest_fer,axis_pos=dest_fer['axis_pos'])
    if verbose: 
        print "Put destination variable"
        pyferret.run('show grid dest')
    
    pyfer_command = 'let result = source[g'+axis.lower()+'=dest'+transform+']'
    pyferret.run(pyfer_command)
    if verbose: 
        print "Regridded in FERRET"
        pyferret.run('show grid result')
    
    # Get results
    result_ref = pyferret.getdata('result')
    if verbose: print "Get data from FERRET"
    # Convert from ferret data structure to util.nc.Variable
    tmp_result = Fer2Num(result_ref)
    if var.has_key('varname'):
        tmp_result['varname'] = var['varname']
    tmp_caxes = [ _NC._assignCAxis_(dimunit) for dimunit in tmp_result['dimunits'] ]
    var_caxes = [ _NC._assignCAxis_(dimunit) for dimunit in var['dimunits'] ]
    # Preserve dimension order (Ferret reverts the order)
    neworder = [ tmp_caxes.index(cax) 
                 for cax in var_caxes ]
    # Change the dimension order of the result to match with the input
    tmp_result['coords'] = [ tmp_result['coords'][iax] for iax in neworder ]
    tmp_result['dimunits'] = [ tmp_result['dimunits'][iax] for iax in neworder ]
    if tmp_result.has_key('dimnames'):
        tmp_result['dimnames'] = [ tmp_result['dimnames'][iax] for iax in neworder ]
    tmp_result['data'] = tmp_result['data'].transpose(neworder).astype(var['data'].dtype)
    # Return the input var with the data and dimensions replaced by the regridded ones
    var.update(tmp_result)
    result = var
    status = pyferret.stop()
    if verbose: 
        if status:
            print "PyFerret stopped."
        else:
            print "PyFerret failed to stop."
    return result
{% endhighlightscroll %}

For some reasons when pyferret is stopped, there is still memory of the previous data grid being kept so that the next time pyferret is started, dimensions of the same names used before (e.g. latitude, longitude) cannot be recognised.  This might be associated with one of the [known issues](http://ferret.pmel.noaa.gov/Ferret/documentation/pyferret/known-issues) of PyFerret.  To work around this, I use the python multiprocessing library to run the PyFerret on a separate process so that each time when the function is finished the process is killed and the memory associated is freed (I think so?).

{% highlightscroll python %}
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

{% endhighlightscroll %}
**2. Interfaces for adding and extracting data to/from PyFerret**

{% highlightscroll python %}
def Num2Fer(var_data,missing_value,coords,dimunits,
            varname="UNKNOWN",data_units=None,cartesian_axes=None,dimnames=None):
    ''' Create a dictionary that resemble the Ferret
	data variable structure to be passed to pyferret.putdata
    Necessary Input:
    var_data       - numpy.ndarray
    missing_value  - float
    coords         - a list of numpy.ndarray
    dimunits       - a list of strings for the dimension units
                     (e.g. ['months','degrees_N','degrees_E'])
    
    Optional input:
    varname        - string
    data_units     - string
    cartesian_axes - a list of characters that specifies
                     the cartesian axes (e.g. ['T','Y','X'])
                     If this is not specified, guesses will be made
                     using the dimension units (say unit month will 
                     be interpreted for a [T]IME axis.  
                     Specifying cartesian_axes overwirtes the 
                     guesses.
    dimnames       - a list of strings for the dimension names
                     (e.g. ['time','lat','lon'])
    
    Length of cartesian_axes, dimnames, dimunits and coords need
    to agree with the number of dimensions of var_data
    
    Return:
    a dictionary
    '''
    import numpy
    import pyferret
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
    if data_units is not None: 
        fer_var['data_unit'] = data_units
    # Determine the axis type
    cax2ax_type = {'X': pyferret.AXISTYPE_LONGITUDE,
                   'Y': pyferret.AXISTYPE_LATITUDE,
                   'Z': pyferret.AXISTYPE_LEVEL,
                   'T': pyferret.AXISTYPE_CUSTOM }
    # Make guessses for the axis type
    if cartesian_axes is None:
        cartesian_axes = [ _assignCAxis_(dimunit) 
                           for dimunit in dimunits ]
    
    if len(cartesian_axes) != var_data.ndim:
        raise Exception("Number of cartesian_axes/dimunits does"+\
                        " not match var_data.ndim")
    
    # Convert it to PyFerret convention
    fer_var['axis_types'] = [ cax2ax_type[cax] 
                              if cax in cax2ax_type.keys() 
                              else pyferret.AXISTYPE_NORMAL
                              for cax in cartesian_axes ]
    
    if dimnames is not None: 
        if len(dimnames) != var_data.ndim:
            raise Exception("Number of dimnames does not match var_data.ndim")
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
{% endhighlightscroll %}


Here I used a function that tries to associate axis type (T/X/Y/Z) to a dimension by utilising the dimension units.

{% highlightscroll python %}
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

{% endhighlightscroll %}


{% highlightscroll python %}
def Fer2Num(var):
    ''' Filter the dictionary returned by pyferret.getdata
    PyFerret usually returns data with extra singlet dimension
    Need to filter those
    Input:
    var       - a dictionary returned by pyferret.getdata
    Return:
    A dictionary with the following items
    data      - a numpy ndarray
    varname   - the name of the variable
    coords    - a list of numpy ndarrays for the dimensions
    dimunits  - a list of strings, the units for the dimensions
    dimnames  - a list of strings, the names for the dimensions
    '''
    import warnings as _warnings
    import numpy
    import pyferret
    results = {}
    results['coords'] = [ ax for ax in var['axis_coords']
                         if ax is not None]
    if var['axis_names'] is not None:
        results['dimnames'] = [ var['axis_names'][i] 
                                for i in range(len(var['axis_names']))
                                if var['axis_coords'][i] is not None ]
    results['dimunits'] = [ var['axis_units'][i] 
                            for i in range(len(var['axis_units']))
                            if var['axis_coords'][i] is not None ]
    
    sliceobj = [ 0 if ax is None else slice(None) 
                 for ax in var['axis_coords'] ]
    results['data'] = var['data'][sliceobj]
    results['varname'] = var['title']
    return results
{% endhighlightscroll %}
