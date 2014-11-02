---
layout: post
category: programming
---

Often when we need to publish a latex article with updated references/citations, we need to run pdflatex and bibtex alternatively for many times.  There must be someone else who did this: write a script to repetively call these two functions.  Unlike you that you have reached this page, I spent some time looking for one and decided that maybe it is quicker to write one myself.

{% highlight bash %}
#!/bin/bash
if [ $1 == '--help' ] || [ $1 == '-h' ]; then
    cat <<EOF
All arguments are passed to pdflatex.
The last argument is assumed to be a tex file and will be passed to bibtex alone.

This script runs bibtex first, and then pdflatex.
If there are warnings indicating missing references or that rerun is needed, bibtex and pdflatex are repeatedly performed until there is no more warning about citations.

Example:
pdfbibtex article.tex

EOF
    exit 0
fi

file_notex=`basename ${@: -1} .tex`

bibtex $file_notex
pdflatex "$@" | tee pdflatex.tmp
if [ -n "`grep "bib Warning" pdflatex.tmp`" ] || [ -n "`grep "Rerun" pdflatex.tmp`"  ]; then
    need_rerun=true
else
    need_rerun=false
fi;
    
while $need_rerun  ; do
    echo "Doing rerun";
    bibtex $file_notex;
    pdflatex "$@" | tee pdflatex.tmp;
    if [ -n "`grep "bib Warning" pdflatex.tmp`" ] || [ -n "`grep "Rerun" pdflatex.tmp`" ]; then
	need_rerun=true
    else
	need_rerun=false
    fi;
done

# Remove the temporary file
rm -f pdflatex.tmp

{% endhighlight %}
