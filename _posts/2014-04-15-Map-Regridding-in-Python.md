---
layout: post
category: programming
---

<!--start-excerpt-->In most cases, both PyFerret and the Python spherical harmonic module (hereafter PySpHarm) do a pretty good job regridding geographical 2D data and preserve area averages.

Here is an example of going from higher resolutions (0.66 degree longitude x 0.5 degree latitude) to lower resolutions (2.66 degree longitude x 2 degree latitude). <!--end-excerpt-->

<a href="/assets/images/regridding_py_lower_crop.png"><img src="/assets/images/regridding_py_lower_crop.png" width="80%"></a>

And this is an example of going from  lower resolutions (2.66 degree longitude x 2 degree latitude) to higher resolutions (0.33 degree longitude x 0.25 degree latitude).

<a href="/assets/images/regridding_py_higher_crop.png"><img src="/assets/images/regridding_py_higher_crop.png" width="80%"></a>
