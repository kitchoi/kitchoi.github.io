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

Here is the code.  Most of it is copied from org.el (version 8.2.10).  It is supposed to overload the original function and therefore it should be included in the .emacs file after org mode and tramp are loaded.

{% highlight lisp %}

(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.
Normally only links without a description part are inlined, because this
is how it will work for export.  When INCLUDE-LINKED is set, also links
with a description part will be inlined.  This can be nice for a quick
look at those images, but it does not reflect what exported files will look
like.
When REFRESH is set, refresh existing images between BEG and END.
This will create new image displays only if necessary.
BEG and END default to the buffer boundaries."
  (interactive "P")
  (when (display-graphic-p)
    (unless refresh
      (org-remove-inline-images)
      (if (fboundp 'clear-image-cache) (clear-image-cache)))
    (save-excursion
      (save-restriction
        (widen)
        (setq beg (or beg (point-min)) end (or end (point-max)))
        (goto-char beg)
        (let ((re (concat "\\[\\[\\(\\(file:\\)\\|\\([./~]\\)\\)\\([^]\n]+?"
                          (substring (org-image-file-name-regexp) 0 -2)
                          "\\)\\]" (if include-linked "" "\\]")))
              (case-fold-search t)
              old file ov img type attrwidth width)
          (while (re-search-forward re end t)
            (setq old (get-char-property-and-overlay (match-beginning 1)
                                                     'org-image-overlay)
                  file (expand-file-name
                        (concat (or (match-string 3) "") (match-string 4))))
            (when (image-type-available-p 'imagemagick)
              (setq attrwidth (if (or (listp org-image-actual-width)
                                      (null org-image-actual-width))
                                  (save-excursion
                                    (save-match-data
                                      (when (re-search-backward
                                             "#\\+attr.*:width[ \t]+\\([^ ]+\\)"
                                             (save-excursion
                                               (re-search-backward "^[ \t]*$\\|\\`" nil t)) t)
                                        (string-to-number (match-string 1))))))
                    width (cond ((eq org-image-actual-width t) nil)
                                ((null org-image-actual-width) attrwidth)
                                ((numberp org-image-actual-width)
                                 org-image-actual-width)
                                ((listp org-image-actual-width)
                                 (or attrwidth (car org-image-actual-width))))
                    type (if width 'imagemagick)))
            (when (file-exists-p file)
              (if (and (car-safe old) refresh)
                  (image-refresh (overlay-get (cdr old) 'display))
  ;; @kychoi: These are the lines I changed
                (progn
                  (if (org-file-remote-p file)
                      (progn
                        (setq tmpdirectory (concat org-babel-temporary-directory "/tramp" (org-file-remote-p file) (file-name-directory (org-babel-local-file-name file))))
                        (make-directory tmpdirectory t)
                        (setq newname (concat tmpdirectory (file-name-nondirectory file)))
                        (if (tramp-handle-file-newer-than-file-p file newname) (tramp-compat-copy-file file newname t t))
                        (setq img (save-match-data (create-image newname type nil :width width))))
  ;; @kychoi: I think I changed up to here
                  (setq img (save-match-data (create-image file type nil :width width)))))
                (when img
                  (setq ov (make-overlay (match-beginning 0) (match-end 0)))
                  (overlay-put ov 'display img)
                  (overlay-put ov 'face 'default)
                  (overlay-put ov 'org-image-overlay t)
                  (overlay-put ov 'modification-hooks
                               (list 'org-display-inline-remove-overlay))
                  (push ov org-inline-image-overlays))))))))))

{% endhighlight %}


I am pushing this change to the org-mode repository.  But for the time being, hopefully this helps someone and may one day make itself into the official release.


