---
layout: post
category: blog
tags: emacs org-mode
---

<!--start-excerpt-->Emacs Org-mode is very powerful tool for scientific research.  You can manage your script, figures and text all in one place.  One problem I have encountered though, is that when I access an .org file with a link to an image hosted on a remote machine, I cannot display that image inline.  Here I posted a solution how you may do it.<!--end-excerpt-->

In other words, in Org-mode, if I have a link like this.

{% highlight lisp %}
[[file:/ssh:user@host:/path/to/image]]
{% endhighlight %}

Then <kbd>Ctrl</kbd>+<kbd>c</kbd> <kbd>Ctrl</kbd>+<kbd>x</kbd> <kbd>Ctrl</kbd>+<kbd>v</kbd> will display an empty box inline.  And there will be an error message.

> Cannot open image file `/ssh:user@host:/path/to/image'

The reason is that <code>org-display-inline-images</code> in Org-mode tries to evaluate the remote file path locally.  Obviously that doesn't work.

Solutions
---

To display the remote image, I modify the <code>org-display-inline-images</code> function so that when it determines that the image file is a remote file, it asks tramp to copy the file to a temporary directory which will be deleted once the emacs buffer is killed.  And to make sure that images of the same names but with different paths do not overwrite each other, the temporary directory tree imitates the remote file path.  Last, org-display-inline-images display the copied, local images inline.

Here is the code.  Most of it is copied from org.el (version 8.3beta).  It is supposed to overload the original function and therefore it should be included in the .emacs file after org mode and tramp are loaded.

{% gist kitchoi/fada6713b96e5d43df18 org.el.diff %}

I am pushing this change to the org-mode repository.  But for the time being, hopefully this helps someone and may one day make itself into the official release.

