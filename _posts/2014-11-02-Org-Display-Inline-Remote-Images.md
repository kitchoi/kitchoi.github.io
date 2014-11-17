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

Here is the code.  Most of it is copied from org.el (version 8.3beta, the function of interest is similar in the stable version 8.2.10).  It is supposed to overload the original function and therefore it should be included in the .emacs file after org mode and tramp are loaded.

{% highlightscroll diff lineno %}
--- a/lisp/org.el
+++ b/lisp/org.el
@@ -19338,7 +19338,7 @@ boundaries."
 			    (not (cdr (org-element-contents parent)))))
 		      (org-string-match-p file-extension-re
 					  (org-element-property :path link)))
-	     (let ((file (expand-file-name (org-element-property :path link))))
+	     (let ((file (substitute-in-file-name (expand-file-name (org-element-property :path link)))))
 	       (when (file-exists-p file)
 		 (let ((width
 			;; Apply `org-image-actual-width' specifications.
@@ -19376,10 +19376,29 @@ boundaries."
 			     'org-image-overlay)))
 		   (if (and (car-safe old) refresh)
 		       (image-refresh (overlay-get (cdr old) 'display))
-		     (let ((image (create-image file
-						  (and width 'imagemagick)
-						  nil
-						  :width width)))
+		     (let* ((image 
+			     (let ((newname
+				    (if (org-file-remote-p file)
+					(let* ((tramp-tmpdir (concat
+							      (if (featurep 'xemacs)
+								  (temp-directory)
+								temporary-file-directory)
+							      "/tramp"
+							      (org-file-remote-p file)
+							      (file-name-directory
+							       (org-babel-local-file-name file))))
+					       (newname (concat
+							 tramp-tmpdir 
+							 (file-name-nondirectory file))))
+					  (make-directory tramp-tmpdir t)
+					  (if (tramp-handle-file-newer-than-file-p file newname)
+						(tramp-compat-copy-file file newname t t))
+					  newname)
+				      file)))
+			       (create-image newname
+					     (and width 'imagemagick)
+					     nil
+					     :width width))))
 		       (when image
 			 (let* ((link
 				 ;; If inline image is the description
{% endhighlightscroll %}

I am pushing this change to the org-mode repository.  But for the time being, hopefully this helps someone and may one day make itself into the official release.

