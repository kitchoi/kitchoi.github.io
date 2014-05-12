---
layout: post
category: programming
---

I just made a new Python class, [TagDict](https://github.com/kitchoi/tagdict), that allows mapping an object to multiple tags and searching for objects that match a list of tags.  Previously I ran into situations in which I used nested Python dictionary (in fact, a recursive [defaultdict](https://docs.python.org/2/library/collections.html)) for my data even though the nested structure is unnecessary.  I realised it would make more sense to manage my data using tags.  But I couldn't find an existing module that I could use, so I figured I would write one myself.  Quite a fun project!

It is on Github (see link above).  Please feel free to use, comment or contribute!

Example
---
{% highlightscroll python %}
data = TagDict()
# 4 unhashable items
data.add({'Name':'Ben'},tags=['Male','Student'])
data.add({'Name':'Tom','Age':40},tags=['Male','Teacher'])
data.add({'Name':'Tina','Age':30},tags=['Female','Teacher'])
data.add({'Name':'Ann'},tags=['Female','Student'])
print data["*"]                     # --> All the items
print data['Teacher','Female']      # --> {'Age': 30, 'Name': 'Tina'}
print data['Student']               # --> ({'Name': 'Ben'}, {'Name': 'Ann'})
# Add one more tag for one of the items
data.add_tag(data['Teacher','Female'],'Mother')
print data['Mother']                # --> {'Age': 30, 'Name': 'Tina'}
# Remove a tag for one of the items
data.remove_tag(data['Male','Student'],'Student')
print data['Student']               # --> {'Name': 'Ann'}
# Replace all the tags of one of the items
data.replace_tags(data['Female','Mother','Teacher'],['Human',])
print data['Mother']                # --> ()
print data['Human']                 # --> {'Age': 30, 'Name': 'Tina'}
# Change the content of one of the items
data['Human']['Age']=31
print data['Human']                 # --> {'Age': 31, 'Name': 'Tina'}
# Remove an item
data.remove(data['Student'])
print data['Student']               # ()
print data['*']                     # Only 3 items now
# Updating a list of items will fail
try:
	data.add_tag(data['Male'],'Martian')
except KeyError:
	print "More than one item were requested."
	for man in data['Male']:
		data.add_tag(man,'Martian')
print data['Martian']              # --> ({'Name': 'Ben'}, {'Age': 40, 'Name': 'Tom'})

{% endhighlightscroll %}
