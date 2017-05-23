+++
date = "2011-07-14T18:44:00.000000+02:00"
title = "Elisp Breadcrumbs"
tags = ["Emacs", "Muse"]
categories = ["Software Programming","Emacs Lisp"]
thumbnailImage = "/img/old/emacs-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/emacs-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/07/14-elisp-breadcrumbs",
           "/blog/2011/07/14-elisp-breadcrumbs.html"]
+++

A 
[breadcrumb](http://en.wikipedia.org/wiki/Breadcrumb_(navigation)) is a navigation aid.  I just added one to this website, so that
it gets easier to browse from any article to its local and parents indexes
and back to 
[/dev/dim](../../../index.html), the root webpage of this site.

As it was not that much work to implement, here's the whole of it:

~~~
;;;
;;; Breadcrumb support
;;;
(defun tapoueh-breadcrumb-to-current-page ()
  "Return a list of (name . link) from the index root page to current one"
  (let* ((current (muse-current-file))
	 (cwd     (file-name-directory current))
	 (project (muse-project-of-file current))
	 (root    (muse-style-element :path (caddr project)))
	 (path    (tapoueh-path-to-root))
	 (dirs    (split-string (file-relative-name current root) "/")))
    ;; ("blog" "2011" "07" "13-back-from-char11.muse")
    (append
     (list (cons "/dev/dim" (concat path "index.html")))
     (loop for p in (butlast dirs)
	   collect (cons p (format "%s%s/index.html" path p))
	   do (setq path (concat path p "/"))))))

(defun tapoueh-insert-breadcrumb-div ()
  "The real HTML inserting"
  (insert "<div id=\"breadcrumb\">")
  (loop for (name . link) in (tapoueh-breadcrumb-to-current-page)
	do (insert (format "<a href=%s>%s</a>" link name) " / "))
  (insert "</div>\n"))

(defun tapoueh-insert-breadcrumb ()
  "Must run with current buffer being a muse article"
  (save-excursion
    (beginning-of-buffer)
    (when (tapoueh-extract-directive "author" (muse-current-file))
      (re-search-forward "<body>" nil t) ; find where the article content is
      (re-search-forward "<h2>" nil t)	 ; that's the title line
      (beginning-of-line)
      (open-line 1)
      (tapoueh-insert-breadcrumb-div)

      (re-search-forward "<h2>" nil t 2) ; that's the TAG line
      (beginning-of-line)
      (open-line 1)
      (tapoueh-insert-breadcrumb-div))))
~~~


This code is now called in the 
`:after` function of my 
[Muse](http://www.emacswiki.org/emacs/EmacsMuse) project style, and
it gets the work done.
