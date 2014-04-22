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
