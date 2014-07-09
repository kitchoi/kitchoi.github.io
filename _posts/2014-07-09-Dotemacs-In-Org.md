---
layout: post
category: programming
---

As time goes, my .emacs file grows so large that I find it difficult to organise, to switch things on/off and document the references where I found solutions for certain tasks.  I ended up using Org to organise my .emacs file so that I can have emacs-lisp code and descriptions all in one file with sections.

My dotemacs.org has two parts.  The first part initialise org so that it can load the rest of itself (the second parts) where most of the settings are.

Initialise org
---

Melpa has to be loaded before Org is loaded.  **This part of the file will be tangled into a file called dotemacs.**

{% highlight lisp %}
* MELPA
#+BEGIN_SRC emacs-lisp :tangle dotemacs
(require 'package)
(add-to-list 'package-archives 
'("melpa" . "http://melpa.milkbox.net/packages/") t)

(when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
#+END_SRC

* Load the rest
#+BEGIN_SRC emacs-lisp :tangle dotemacs
(require 'org)
(org-babel-load-file "~/env/emacs/dotemacs.org")
#+END_SRC

{% endhighlight %}

The rest of the dotemacs.org file
---
You put everything else here.  **This part of the file will be tangled into a file called dotemacs.el**

{% highlight lisp %}
* Python
** Jedi
Description for Jedi.  Link to Jedi official page.  Whatever comments you want.
#+BEGIN_SRC emacs-lisp :tangle yes
(require 'jedi)
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
#+END_SRC

* Magit
#+BEGIN_SRC emacs-lisp :tangle yes
(global-set-key (kbd "C-c g") 'magit-status)
#+END_SRC
{% endhighlight %}


Last step
---

The first part of the org file is tangled into a file called "dotemacs" (because I put "dotemacs" after :tangle).  Now all you need to do is to make a symbolic link in your home directory to point to this dotemacs file.  Then emacs will load the dotemacs.org to tangle the rest of the settings into dotemacs.el.  Org is smart enough not to recompile dotemacs.el if nothing is changed.

{% highlight bash %}
ln -s ~/env/emacs/dotemacs ~/.emacs 
{% endhighlight %}

Alternative method - not using Org at all
---

Another way to do it would be to put settings into separate .el files and include them using the <code>require</code> command in your .emacs file like this:


{% highlight lisp %}
(add-to-list 'load-path "~/.emacs.d/user_settings")

(require 'python-settings)
{% endhighlight %}

And you will have your python-mode setting in <file>python-settings.el</file> (and many others) under ~/.emacs.d/user_settings

