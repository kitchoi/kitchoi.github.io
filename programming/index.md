---
layout: master
title: Programming
is_blog: true
---

<ul>
  {% for post in site.categories.programming %}
    <li>
      <a href="{{ post.url }}">{{ post.title }}</a>
    </li>
  {% endfor %}
</ul>
