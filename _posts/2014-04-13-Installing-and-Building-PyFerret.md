---
layout: post
category: blog
tags: python
most_popular: true
---

I can use NumPy and SciPy for most of the tasks in my work, except for regridding.  Writing my own regridding function is tedious and time-consuming and I would likely make mistakes that can be avoided by using existing packages.  My first work-around was to use the [Python spherical harmonic transform module](https://code.google.com/p/pyspharm/) since it is a python package that is pretty easy to install and to use.  It has served me well until at certain point the Gibbs oscillations innate to the transformation starts to hamper the quality of my analysis. <!--start-excerpt-->
 I needed regridding functions that can conserve area averages and do not generate the artificial Gibbs fringes.  There are lots of options, and Ferret/[PyFerret](http://ferret.pmel.noaa.gov/Ferret/documentation/pyferret) being one.  And here I present how to get PyFerret and its dependencies installed.  <!--end-excerpt-->  In the next post I show how to use it as an external package from Python.

Before installing PyFerret, we need to install ESMF and ESMP.  The challenging bits came when installing PyFerret itself.


Download ESMF and ESMP
-------------------------------
You can download the ESMF and ESMP altogether from the [Download and Releases](https://earthsystemcog.org/projects/esmp/releases) page of ESMP.  The following procedures were performed with the ESMF_6_2_0_ESMP_01 version.

Install ESMF
-------------------------------
ESMF does provide a pretty good user guide ([version 6.2.0](http://www.earthsystemmodeling.org/esmf_releases/non_public/ESMF_6_2_0/ESMF_usrdoc/) and [version 6.3.0](http://www.earthsystemmodeling.org/esmf_releases/public/ESMF_6_3_0r/ESMF_usrdoc/)).  The steps I did to install ESMF:

**1. Extract the tarball** into /path/to/esmp.ESMF_6_2_0_ESMP_01

In this directory you should find two directories: **esmf** and **ESMP**.  Now we work at the **esmf** directory.
{% highlight bash %}
cd /path/to/esmp.ESMF_6_2_0_ESMP_01/esmf
{% endhighlight %}

**2. Get the g++ compiler.**

On Fedora, it is
{% highlight bash %}
sudo yum install gcc-c++
{% endhighlight %}

**3. Set environment variables.**

Assuming you are at the source code directory (you should find the LICENSE, README, makefile etc.)
{% highlight bash %}
export ESMF_DIR=$PWD
{% endhighlight %}

I prefer having the esmf library installed somewhere under my home directory that does not get synced with my other machines.  I created a directory and assigned ESMF_INSTALL_PREFIX to it.
{% highlight bash %}
export ESMF_INSTALL_PREFIX=$HOME/Enthought/lib/python2.7/esmf
{% endhighlight %}

**4. Gmake.**

Simply install it using gmake.
{% highlight bash %}
gmake
gmake install
{% endhighlight %}


Install ESMP
---
Steps:

**1. Set environment variables**

After installing ESMF, you should find the esmf.mk makefile at $ESMF_INSTALL_PREFIX/lib/.../esmf.mk.  Set ESMFMKFILE to it.

{% highlight bash %}
export ESMFMKFILE=$HOME/Enthought/lib/python2.7/esmf/lib/libO/Linux.gfortran.64.mpiuni.default/esmf.mk
{% endhighlight %}

**2. Build it**

In the **ESMP** directory,
{% highlight bash %}
cd /path/to/esmp.ESMF_6_2_0_ESMP_01/ESMP
gmake build
{% endhighlight %}

If there is no error then you are done.

**3. Add the ESMP to PYTHONPATH.**

Before running the test scripts, add the path of the ESMP directory to your global PYTHONPATH and not just the current session because the test examples run on another python shell.
{% highlight bash %}
export PYTHONPATH=/path/to/esmp.ESMF_6_2_0_ESMP_01:$PYTHONPATH
{% endhighlight %}


Download and Install PyFerret
---
You can download PyFerret from [here](http://ferret.pmel.noaa.gov/Ferret/downloads/pyferret/).  If you are lucky, you may use the binaries provided then you may very much skip the pain of building it from the source code.  I wasn't and here were the steps I followed.  The version I used was pyferret-1.0.0.  *(Update: I tested with pyferret-1.0.2 too.)*

**1. Extracting tarball**

Let's say we have extracted the tarball into *$HOME/Downloads/pyferret-1.0.0-source*

**2. Install development packages.**

On Fedora, I needed netcdf, hdf5, cairo, java-openjdk and pango development packages.
{% highlight bash %}
sudo yum install netcdf-fortran-devel cairo-devel java-1.7.0-openjdk-devel pango-devel
{% endhighlight %}

If you are using other operating systems such as Ubuntu, the packages names can be found on [Installing or Building PyFerret](http://ferret.pmel.noaa.gov/Ferret/documentation/pyferret/build-install/)


**3. Modify ~/Downloads/pyferret-1.0.0-source/site_specific.mk**

Depending on your setup and preferences, change the followings:
- DIR_PREFIX 	    = $(HOME)/Downloads/pyferret-1.0.0-source
- BUILDTYPE	        = x86_64-linux
- PYTHON_EXE	    = python2.7
- INSTALL_FER_DIR	= $(DIR_PREFIX)/ferret_build  #This is where ferret will be built to
- CAIRO_DIR	        =                             #blank
- HDF5_DIR	        = /usr
- NETCDF4_DIR	    = /usr
- JAVA_HOME	        = /usr/lib/jvm/java-openjdk


**4. Modify ~/Downloads/pyferret-1.0.0-source/external_functions/ef_utility/site_specific.mk**

Yes.  There is a second makefile to modify.  Again, change the following variables according to your setup.

- BUILDTYPE  = x86_64-linux
- PYTHON_EXE = python2.7


**5. Modify source code (Hack)**

I couldn't compile until I made the following changes.  This is likely to be a hack.  PyFerret has not crashed so far.

In *~/Downloads/pyferret-1.0.0-source/fer/common/EF_mem_subsc.cmn* **and** *~/Downloads/pyferret-1.0.0-source/external_functions/ef_utility/ferret_cmn/EF_mem_subsc.cmn*
{% highlight fortran %}
	!EXTERNAL FERRET_EF_MEM_SUBSC                  !I commented this
{% endhighlight %}

*Update: With pyferret-1.0.2 version, the first file is fixed but not the second file.*

**6. Make install**

{% highlight bash %}
make
make install
{% endhighlight %}

If the compiler complains about not being able to find *lib\*.a* or *lib\*.o*, either the directory you provided (e.g. /usr ) for that library is incorrect or the library binary (libnetcdf.o) is actually there but has a different file suffix (e.g. *\*.so* instead of *\*.a*), just create symbolic links to fix it.


**7. Copy the pyferret directory to wherever you want**

If the pyferret python site packages are all you need, you can copy the packages to wherever convenient.  I prefer having it in my work library.
{% highlight bash %}
cp -r ~/Downloads/pyferret-1.0.0-source/ferret_build/lib/python2.7/site-packages/* $WORK_LIB/pyferret-lib/
{% endhighlight %}

Or you can make symbolic links.  But from now on I can delete ~/Downloads/pyferret-1.0.0-source whenever I wish.

**8. Set PYTHONPATH**

{% highlight bash %}
export PYTHONPATH=$WORK_LIB/pyferret-lib:$PYTHONPATH
{% endhighlight %}

Now when you are in a python shell, you should be able to import pyferret
{% highlight python %}
import pyferret
{% endhighlight %}
