---
layout: post
category: programming
---

I just made a new Python class, [TagDict](https://github.com/kitchoi/tagdict), that allows mapping an object to multiple tags and searching for objects that match a list of tags.  Previously I ran into situations in which I used nested Python dictionary (in fact, a recursive [defaultdict](https://docs.python.org/2/library/collections.html)) for my data even though the nested structure is unnecessary.  I realised it would make more sense to manage my data using tags.  But I couldn't find an existing module that I could use, so I figured I would write one myself.  Quite a fun project!

Example
---

**Initialise a dataset**

{% highlight python %}
data = TagDict()
# 4 unhashable items
data.add({'Name':'Ben'},tags=['Male','Student'])
data.add({'Name':'Tom','Age':40},tags=['Male','Teacher'])
data.add({'Name':'Tina','Age':30},tags=['Female','Teacher'])
data.add({'Name':'Ann'},tags=['Female','Student'])
{% endhighlight %}

**Retrieve content using tag**

{% highlight python %}
print data["*"]                     # --> All the items
print data['Teacher','Female']      # --> {'Age': 30, 'Name': 'Tina'}
print data['Student']               # --> ({'Name': 'Ben'}, {'Name': 'Ann'})
{% endhighlight %}

**Add a tag to an item**

{% highlight python %}
data.add_tag(data['Teacher','Female'],'Mother')
print data['Mother']                # --> {'Age': 30, 'Name': 'Tina'}
{% endhighlight %}

**Remove a tag from an item**

{% highlight python %}
data.remove_tag(data['Male','Student'],'Student')
print data['Student']               # --> {'Name': 'Ann'}
{% endhighlight %}

**Replace all the tags of an item**

{% highlight python %}
data.replace_tags(data['Female','Mother','Teacher'],['Human',])
print data['Mother']                # --> ()
print data['Human']                 # --> {'Age': 30, 'Name': 'Tina'}
{% endhighlight %}

**Change the content of an item**

{% highlight python %}
data['Human']['Age']=31
print data['Human']                 # --> {'Age': 31, 'Name': 'Tina'}
{% endhighlight %}

**Remove an item**

{% highlight python %}
data.remove(data['Student'])
print data['Student']               # ()
print data['*']                     # Only 3 items now
{% endhighlight %}

**One item only**

To prevent unintended change of tags, all functions that modify tags only accept one item to make sure you know which item you are modifying.

{% highlight python %}
# Updating a list of items will fail
try:
	data.add_tag(data['Male'],'Martian')
except KeyError:
	print "More than one item were requested."
	for man in data['Male']:
		data.add_tag(man,'Martian')
print data['Martian']              # --> ({'Name': 'Ben'}, {'Age': 40, 'Name': 'Tom'})

{% endhighlight %}


**View all items**

{% highlight python %}
print data.view_all()  # --> (({'Name': 'Ben'}, set(['Male', 'Martian'])), ({'Age': 40, 'Name': 'Tom'}, set(['Male', 'Martian', 'Teacher'])), ({'Age': 31, 'Name': 'Tina'}, set(['Human'])))
{% endhighlight %}
