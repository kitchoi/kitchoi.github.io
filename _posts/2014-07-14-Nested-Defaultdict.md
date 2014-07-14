---
layout: post
category: programming
---

The collections.defaultdict has been very useful and it saves me from having a lot of try-except blocks when dealing with dictionaries.  Singly nested dictionary can be done by `collections.defaultdict(collections.defaultdict)`.  But what about multiple layer nested dictionary?

Essentially I want to do this:

{% highlight python %}
data_dict = nested_defaultdict()
data_dict['A']['B']['C']['D'] = 1
{% endhighlight %}

without having to initialise `data_dict['A']`, `data_dict['A']['B']` etc.

After some testing (mainly to make sure the new class is pickle-able), I ended up with this:

{% highlight python %}
import collections
import warnings


class nested_defaultdict(collections.defaultdict):
    def __init__(self,default_factory=None):
        if default_factory is None:
            # recursively use itself
            self.default_factory = nested_defaultdict
        else:
            # when loading from pickled object,
            # the default_factory is not None but the nested_defaultdict
            self.default_factory = default_factory
            if default_factory != nested_defaultdict:
                warnings.warn('nested_defaultdict is made to behave'+\
                                  'like the original defaultdict')


{% endhighlight %}

