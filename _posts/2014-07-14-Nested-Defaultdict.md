---
layout: post
category: programming
---

The collections.defaultdict has been very useful and it saves me from having a lot of try-except blocks when dealing with dictionaries.  Singly nested dictionary can be done by `collections.defaultdict(collections.defaultdict)`.  But what about multi-layer nested dictionary?

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

