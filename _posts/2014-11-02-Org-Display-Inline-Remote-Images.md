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

To display the remote image, I modify the <code>org-display-inline-images</code> function so that when it determines that the image file is a remote file, it asks tramp to copy the file to the default temporary directory (usually "/tmp" unless it is set to something else).  And to make sure that images of the same names but with different paths do not overwrite each other, the temporary directory tree imitates the remote file path.  Last, org-display-inline-images display the copied, local images inline.

Here is the code "diff-ed" from org.el (version 8.3beta).

{% highlight diff lineno %}
--- a/lisp/org.el
+++ b/lisp/org.el
@@ -19340,7 +19340,7 @@ boundaries."
 			    (not (cdr (org-element-contents parent)))))
 		      (org-string-match-p file-extension-re
 					  (org-element-property :path link)))
-	     (let ((file (expand-file-name (org-element-property :path link))))
+	     (let ((file (substitute-in-file-name (expand-file-name (org-element-property :path link)))))
 	       (when (file-exists-p file)
 		 (let ((width
 			;; Apply `org-image-actual-width' specifications.
@@ -19378,10 +19378,25 @@ boundaries."
 			     'org-image-overlay)))
 		   (if (and (car-safe old) refresh)
 		       (image-refresh (overlay-get (cdr old) 'display))
-		     (let ((image (create-image file
-						  (and width 'imagemagick)
-						  nil
-						  :width width)))
+		     (let ((image 
+			    (create-image (if (org-file-remote-p file)
+					      (let* ((tramp-tmpdir (concat
+								    (if (featurep 'xemacs)
+									(temp-directory)
+								      temporary-file-directory)
+								    "/tramp"
+								    (file-name-directory (expand-file-name file))))
+						     (newname (concat
+							       tramp-tmpdir 
+							       (file-name-nondirectory (expand-file-name file)))))
+						(make-directory tramp-tmpdir t)
+						(if (file-newer-than-file-p file newname)
+						    (copy-file file newname t t))
+						newname)
+					    file)
+					  (and width 'imagemagick)
+					  nil
+					  :width width)))
 		       (when image
 			 (let* ((link
 				 ;; If inline image is the description

{% endhighlight %}

For completeness, here is the function as a whole.  It is supposed to overload the original function and therefore it should be included in the .emacs file after org mode and tramp are loaded.  New release may not be compatible.  Use with caution.

{% highlight lisp %}
(defun org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.

An inline image is a link which follows either of these
conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.  BEG and END default to the buffer
boundaries."
  (interactive "P")
  (when (display-graphic-p)
    (unless refresh
      (org-remove-inline-images)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (org-with-wide-buffer
     (goto-char (or beg (point-min)))
     (let ((case-fold-search t)
	   (file-extension-re (org-image-file-name-regexp)))
       (while (re-search-forward "[][]\\[\\(?:file\\|[./~]\\)" end t)
	 (let ((link (save-match-data (org-element-context))))
	   ;; Check if we're at an inline image.
	   (when (and (equal (org-element-property :type link) "file")
		      (or include-linked
			  (not (org-element-property :contents-begin link)))
		      (let ((parent (org-element-property :parent link)))
			(or (not (eq (org-element-type parent) 'link))
			    (not (cdr (org-element-contents parent)))))
		      (org-string-match-p file-extension-re
					  (org-element-property :path link)))
	     (let ((file (substitute-in-file-name (expand-file-name (org-element-property :path link)))))
	       (when (file-exists-p file)
		 (let ((width
			;; Apply `org-image-actual-width' specifications.
			(cond
			 ((not (image-type-available-p 'imagemagick)) nil)
			 ((eq org-image-actual-width t) nil)
			 ((listp org-image-actual-width)
			  (or
			   ;; First try to find a width among
			   ;; attributes associated to the paragraph
			   ;; containing link.
			   (let ((paragraph
				  (let ((e link))
				    (while (and (setq e (org-element-property
							 :parent e))
						(not (eq (org-element-type e)
							 'paragraph))))
				    e)))
			     (when paragraph
			       (save-excursion
				 (goto-char (org-element-property :begin paragraph))
				   (when
				       (re-search-forward
					"^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"
					(org-element-property
					 :post-affiliated paragraph)
					t)
				     (string-to-number (match-string 1))))))
			   ;; Otherwise, fall-back to provided number.
			   (car org-image-actual-width)))
			 ((numberp org-image-actual-width)
			  org-image-actual-width)))
		       (old (get-char-property-and-overlay
			     (org-element-property :begin link)
			     'org-image-overlay)))
		   (if (and (car-safe old) refresh)
		       (image-refresh (overlay-get (cdr old) 'display))
		     (let ((image 
			    (create-image (if (org-file-remote-p file)
					      (let* ((tramp-tmpdir (concat
								    (if (featurep 'xemacs)
									(temp-directory)
								      temporary-file-directory)
								    "/tramp"
								    (file-name-directory (expand-file-name file))))
						     (newname (concat
							       tramp-tmpdir 
							       (file-name-nondirectory (expand-file-name file)))))
						(make-directory tramp-tmpdir t)
						(if (file-newer-than-file-p file newname)
						    (copy-file file newname t t))
						newname)
					    file)
					  (and width 'imagemagick)
					  nil
					  :width width)))
		       (when image
			 (let* ((link
				 ;; If inline image is the description
				 ;; of another link, be sure to
				 ;; consider the latter as the one to
				 ;; apply the overlay on.
				 (let ((parent
					(org-element-property :parent link)))
				   (if (eq (org-element-type parent) 'link)
				       parent
				     link)))
				(ov (make-overlay
				     (org-element-property :begin link)
				     (progn
				       (goto-char
					(org-element-property :end link))
				       (skip-chars-backward " \t")
				       (point)))))
			   (overlay-put ov 'display image)
			   (overlay-put ov 'face 'default)
			   (overlay-put ov 'org-image-overlay t)
			   (overlay-put
			    ov 'modification-hooks
			    (list 'org-display-inline-remove-overlay))
			   (push ov org-inline-image-overlays)))))))))))))))

{% endhighlight %}

I am pushing my work to the org-mode repository.  But for the time being, hopefully this helps someone and may one day make itself into the official release.

