---
layout: master
title: Activities
is_blog: true
---

<ul>
  {% for post in site.categories.activities %}
	<li>
      <a href="{{ post.url }}">{{ post.title }}</a>
	</li>
  {% endfor %}
</ul>
