---
layout: master
category: Programming
---

Motivation
---
I can use NumPy and SciPy for most of the task in my work, except for regridding.  Writing my own regridding function is tedious and time-consuming and I would likely make mistakes that can be avoided by using existing packages.  My first work-around was to use the [Python spherical harmonic transform module](https://code.google.com/p/pyspharm/) since it is a python package that is pretty easy to install and to use.  It has served me well until at certain point the ripple pattern innate to the transformation starts to hamper the quality of my analysis. <!--start-excerpt-->
 I needed regridding functions that can conserve area averages and do not generate ripples.  The [Earth System Modeling Framework (ESMF)](http://www.earthsystemmodeling.org/) and its [Python interface (ESMP)] (https://earthsystemcog.org/projects/esmp/) were the next things I tried.  Unfortunately there were a lot of overhead in order to use them.  Fortunately, there are other Python-based scientific packages such as UV-CDAT, PyFerret and Community Surface Dynamics Modeling System (CSDMS) that have used ESMF and ESMP.  I have used Ferret and UV-CDAT before and I decided that I would try to install PyFerret and use it as an external package in my own library.  Before installing PyFerret, we need to install ESMF and ESMP.
<!--end-excerpt-->

Installing ESMF
---





