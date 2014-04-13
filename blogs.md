---
layout: master
---

{% for category in site.categories %}
<b>{{ category[0] | capitalize }}</b>
<ul>
{% for post in category[1] %}
<li><a href="{{ post.url }}">{{ post.title }}</a> - <time>{{ post.date | date: "%e %B %Y" }}</time></li>
{% endfor %}
</ul>
{% endfor %}
