---
layout: master
---


{% for post in site.posts %}
	{% if post.categories[0] == recipes' %}
<h3><a href="{{ post.url }}">{{ post.title }}</a></h3>
<p>
{{ post.excerpt}}
in <a href='/{{ post.categories[0] }}'>{{ post.categories[0] | capitalize }}</a> - <time>{{ post.date | date: "%e %B %Y" }}</time>
</p>
	{% endif %}
{% endfor %}
