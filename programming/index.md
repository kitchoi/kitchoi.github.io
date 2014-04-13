---
layout: master
title: Programming
is_blog: true

---

<ul>
  {% for post in site.categories.programming %}
    <li>
      <span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ post.url }}">{{ post.title }}</a>
    </li>
  {% endfor %}
</ul>
