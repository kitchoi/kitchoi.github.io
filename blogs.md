---
layout: master
---


{% for post in site.posts %}
<h3><a href="{{ post.url }}">{{ post.title }}</a></h3>
<p>
{% assign contentsize =  post.content | size  %}
{% capture shortcontent %}{{ post.content | split: '<!--start-excerpt-->' | last | split: '<!--end-excerpt-->' | first }}{% endcapture %}
{% assign shortcontentsize = shortcontent | size  %}
{{ shortcontent }}
{% if shortcontentsize < contentsize %}
<a href="{{ post.url }}">Read more</a>
{% endif %}
</p>
<p>
in <a href='/{{ post.categories[0] }}.html'>{{ post.categories[0] | capitalize }}</a> - <time>{{ post.date | date: "%e %B %Y" }}</time>
</p>
{% endfor %}
