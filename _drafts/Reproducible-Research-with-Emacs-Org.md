---
layout: post
category: blog
published: false
tags: emacs org-mode python
---

<!--start-excerpt-->From scratch to publications, you can manage your source data, analysis code, figures, TODO lists and manuscripts within a single text file using Emacs Org-mode.  You can ensure yourself and your colleague that your research is documented and reproducible.
<!--end-excerpt--> In this post I will show you step-by-step how to

1. Setup Org
1. Org Basics
1. Do analysis using code blocks
1. Import data tables for code blocks
1. Run code blocks on remote machines
1. Embed figures and display them inline
1. Cross references
1. Citations
1. Export to LaTeX

Code block examples are Python, but Org-mode does support a number of widely-use languages such as R, Matlab and Octave.  See a full list of supported languages [here](http://orgmode.org/worg/org-contrib/babel/languages.html).

Setup Org
---

1. Download the [latest stable version of Org-mode](http://orgmode.org/org-latest.tar.gz) and unpack the tarball.
    
	Though Emacs is now shipped with Org, at the time of writing, the default Org-mode is version 7.9.3f, with which we would run into errors when we run code block in a live session.  The following demonstration is done using Emacs 24.3 and Org-mode 8.2.10 (Oct 2014).

2. Set the path for emacs to load it (otherwise Emacs would just load the default Org-mode).   Add the following in ~/.emacs

	{% highlightscroll lisp %}
    (add-to-list 'load-path "/path/to/org-8.2.10/lisp")
    (require 'org)
    {% endhighlightscroll %}

3. Restart your emacs


Org Basics
---

If you have used Org for other purposes, you may skip this section and move onto the next one.

###Section

To start a section, simply use "*" as the first character in a line.  Use "\*\*" for subsection, "\*\*\*" and so on.  <kbd>Tab</kbd> can be used to fold up sections.

Example:
<pre>
* Section 1
  ** Subsection 1
  ** Subsection 2
</pre>

###Table

Tables are useful for displaying data, organising where your data is, as well as doing some simple spreadsheet math/stat operation.

Example:
<pre>
| Experiment name | Description |
|-
| Contrl  | Control experiment |
| Pert | Perturbed experiment |
</pre>

When you do <kbd>Ctrl</kbd>+<kbd>c</kbd> or <kbd>Tab</kbd> anywhere in the table, Org will align the rows and columns for you:

<pre>
| Experiment name | Description          |
|-----------------+----------------------|
| Contrl          | Control experiment   |
| Pert            | Perturbed experiment |
</pre>

###Link

Link to a file example:
<pre>
[[file:~/image.png]]
</pre>
If you do <kbd>Ctrl</kbd>+c <kbd>Ctrl</kbd>+<kbd>x</kbd> <kbd>Ctrl</kbd>+<kbd>v</kbd>, the image can be displayed inline (seriously useful; press the sequence again to turn off).

Web hyperlink example:
<pre>
[[http://url]]
</pre>

Do analysis using code blocks
---

Every code block starts with <code>#+BEGIN_SRC</code> and ends with <code>#+BEGIN_SRC</code>.  The first word following BEGIN_SRC specifies the language of the code.

<pre>
#+BEGIN_SRC python :session *py-session* :dir /ssh:user@host:/path/to/run
import pylab
import scipy.io.netcdf as netcdf

ncfile = netcdf.netcdf_file('/work/demo/sample.nc')
tmp = ncfile.variables['TMP'].data
pylab.plot(tmp)  # plot the data
pylab.savefig('/work/demo/figures/python_demo.png') # Save figure

#+END_SRC
</pre>
