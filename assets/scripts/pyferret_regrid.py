# The MIT License (MIT)

# Copyright (c) 2004, Kit-Yan Choi

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from functools import wraps

def regrid_once_primitive(var,ref_var,axis,
                          verbose=False,prerun=None,transform='@ave'):
    ''' Now only deal with regridding without the time axis
    Input:
    var       - a dictionary contains arguments for Num2Fer
                Requires: data,coords,dimunits
    ref_var   - a dictionary contains arguments for Num2Fer. 
                This supplies the grid for regridding
                Requires: coords,dimunits
    axis      - the axis for regridding, it can be any combination of X/Y/Z, e.g. 'XY','Y','Z'
    verbose   - whether to print progress (default: False)
    prerun    - a list of strings as commands to be run at the start (default: None)
    transform - @ave (Conserve area average),@lin (Linear interpolation),...see Ferret doc
    Return:
    a dictionary 
    '''
    import pyferret
    import numpy
    import util.nc as _NC
    if type(var) is not dict:
        raise TypeError("var should be a dictionary.")
    if type(ref_var) is not dict:
        raise TypeError("ref_var should be a dictionary.")
    for var_key in ['data','coords','dimunits']:
        if var_key not in var:
            raise Exception(var_key+" is required in var")
        if var_key != 'data' and var_key not in ref_var:
            raise Exception(var_key+" is required in ref_var")
    
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
    # Make sure axis is a string denoting X or Y or Z axis
    for ax in axis:
        if ax not in ['X','Y','Z']:
            raise Exception("Currently axis can only be any combination of X/Y/Z")
    
    # Construct the source data read by pyferret.putdata
    source_fer = Num2Fer(varname="source",**var)
    
    # Fill in unnecessary input for Ferret
    if not ref_var.has_key('data'):
        ref_var['data'] = numpy.zeros([ len(coord) for coord in ref_var['coords']])
    
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

def Num2Fer(data,coords,dimunits,
            varname="UNKNOWN",data_units=None,cartesian_axes=None,dimnames=None,missing_value=None):
    ''' Create a dictionary that resemble the Ferret
	data variable structure to be passed to pyferret.putdata
    Necessary Input:
    data       - numpy.ndarray
    coords         - a list of numpy.ndarray
    dimunits       - a list of strings for the dimension units
                     (e.g. ['months','degrees_N','degrees_E'])
    
    Optional input:
    varname        - string
    data_units     - string
    missing_value  - numeric
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
    to agree with the number of dimensions of data
    
    Return:
    a dictionary
    '''
    import numpy
    import pyferret
    if len(dimunits) != data.ndim:
        raise Exception("Number of dimunits does not match data.ndim")
    if len(coords) != data.ndim:
        raise Exception("Number of coords does not match data.ndim")
    fer_var = {}
    # Define the variable
    fer_var['data'] = data.copy()
    # Variable name
    fer_var['name'] = varname
    # Dataset
    fer_var['dset'] = None
    # Title = variable name
    fer_var['title'] = fer_var['name']
    # Set missing value
    if missing_value is not None:
        fer_var['missing_value'] = missing_value
    else:
        fer_var['missing_value'] = numpy.ma.default_fill_value(fer_var['data'])
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
    
    if len(cartesian_axes) != data.ndim:
        raise Exception("Number of cartesian_axes/dimunits does"+\
                        " not match data.ndim")
    
    # Convert it to PyFerret convention
    fer_var['axis_types'] = [ cax2ax_type[cax] 
                              if cax in cax2ax_type.keys() 
                              else pyferret.AXISTYPE_NORMAL
                              for cax in cartesian_axes ]
    
    if dimnames is not None: 
        if len(dimnames) != data.ndim:
            raise Exception("Number of dimnames does not match data.ndim")
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
### work on my machine for making plots
    # import util
    # import pylab
    # var1 = util.nc.Variable(data=var['data'],dims=[ util.nc.Dimension(data=var['coords'][i],units=var['dimunits'][i]) for i in range(len(var['coords']))],varname='var')
    # result1 = util.nc.Variable(data=result['data'],dims=[ util.nc.Dimension(data=result['coords'][i],units=result['dimunits'][i]) for i in range(len(result['coords']))],varname='result')
    # pylab.figure()
    # pylab.subplot(2,1,1)
    # util.nc.pcolor(var1,meridians=[100.,120.,140.,160.])
    # pylab.title("Original")
    # pylab.subplot(2,1,2)
    # util.nc.pcolor(result1,meridians=[100.,120.,140.,160.])
    # pylab.title("Regridded")
    # pylab.savefig("../images/pyferret_regrid_example.png")
