---
layout: master
---


{% for post in site.posts %}
	{% if post.categories[0] == 'programming' %}
<h3><a href="{{ post.url }}">{{ post.title }}</a></h3>
<p>
{{ post.content | split: '<!--start-excerpt-->' | last | split: '<!--end-excerpt-->' | first }} <a href="{{ post.url }}">[Read more]</a>
</p>
<p>
in <a href='/{{ post.categories[0] }}.html'>{{ post.categories[0] | capitalize }}</a> - <time>{{ post.date | date: "%e %B %Y" }}</time>
</p>
	{% endif %}
{% endfor %}
