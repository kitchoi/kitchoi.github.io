---
layout: post
category: blog
tags: python
---

<div class="alert alert-warning" role="alert"> <b>Warning</b> If KeyError is useful for your debugging, defaultdict may not be something you want to use. </div>

<!--start-excerpt-->The collections.defaultdict is very useful for well tested code as it cleans up a lot of try-except blocks when dealing with dictionaries.  Singly nested dictionary can be done by `collections.defaultdict(collections.defaultdict)`.  But what about multi-layer nested dictionary?<!--end-excerpt-->

Essentially I want to do this:

{% highlight python %}
data_dict = nested_defaultdict()
data_dict['A']['B']['C']['D'] = 1
{% endhighlight %}

without having to initialise `data_dict['A']`, `data_dict['A']['B']` etc.

A recursive function would do.  And this is pickle-able too.

{% highlight python %}
import collections


def nested_defaultdict():
    return collections.defaultdict(nested_defaultdict)

{% endhighlight %}

**Update (Dec 8, 2014):** I just learned that this is called [autovivification](http://en.wikipedia.org/wiki/autovivification).  Very interesting.  The Python example there is basically the same as the one presented here.
