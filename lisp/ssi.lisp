;;;; ssi-lisp.lisp
;;;
;;; The lisp software publishing http://tapoueh.org
;;;
;;; Here we read HTML files and replace <lisp> tags with the result of
;;; evaluating the code they contain.

(in-package #:tapoueh)

(defparameter *base-url* "http://tapoueh.org/"
  "Only used to publish in the headers Meta.")

(defvar *muse-current-file* nil
  "Maintained in the web.lisp hunchentoot handlers.")

(defvar *host* "tapoueh.org" "Service hostname")
(defvar *script-name* nil "script-name part of the URL currently requested")


(def-suite ssi :description "HTML SSI Parser Test Suite.")
(in-suite ssi)

;;
;; we use cl-pprce:regexp-replace facilities, in particular the replacement
;; function in simple-calls mode.
;;
(defun replace-<lisp> (match &rest registers)
  "Eval a <lisp>(code)</lisp> code block, returns its result as a string.

   The code can return a string, which is taken as is, or a list, which is
   then given to the cl-who package for HTML rendering of it. Anything else
   is post-processed with (format nil \"~a\" ...)."
  (declare (ignore match))
  (let ((result (eval (let ((*package* (find-package 'tapoueh)))
			(read-from-string (first registers))))))
    (if result
	(typecase result
	  (string result)
	  (list   (eval `(with-html-output-to-string (s) ,result)))
	  (t      (format nil "~a" result)))
	;; result is null, return an empty string
	"")))

(defun eval-lisp-tags (string)
  "Replace each <lisp>(code)</lisp> by the result of evaluating the code."
  (ppcre:regex-replace-all "<lisp>(.*)</lisp>" string #'replace-<lisp>
			   :simple-calls t))

#+5am
(test replace-ssi-lisp-tags
      "Test parsing a Server Side Include: html with <lisp>(code)</lisp>"
      (is (string= (eval-lisp-tags "<lisp>(+ 1 2)</lisp>") "3"))
      (is (string= (eval-lisp-tags "<html><lisp>(+ 1 2)</lisp></html>")
		   "<html>3</html>"))
      (is (string= (eval-lisp-tags "<lisp>'(:a :href \"/\" \"slash\")</lisp>")
		   "<a href='/'>slash</a>"))
      (is (string= (eval-lisp-tags "<lisp>(tapoueh-style-sheet)</lisp>")
		   "<link rel='stylesheet' type='text/css' media='all' href='/static/styles.css' />")))

(defun ssi-file (pathname)
  "Server Side Include file at given PATHNAME, replacing <lisp> tags"
  (eval-lisp-tags (slurp-file-into-string pathname)))

;;;
;;; Helper functions used in the SSI directives
;;;
;;; Compatibility layer kept to be able to publish the website in a
;;; subdirectory somewhere, for now just spit back a / based URL.
;;;
(defun tapoueh-style-sheet ()
  "Return the link to the main CSS"
  '(:link
    :rel "stylesheet"
    :type "text/css"
    :media "all"
    :href "/static/styles.css"))

(defun tapoueh-rss-index ()
  "Get the relative link to the RSS feed"
  "/rss/tapoueh.xml")

(defun tapoueh-contents ()
  "Output the link to the /contents.html page"
  "/contents.html")

(defun tapoueh-root-index ()
  "Output the link to the /contents.html page"
  "/index.html")

(defun tapoueh-2ndquadrant-logo ()
  "Get the :style-sheet property and rework the link to the CSS"
  '(:img :src "/static/2ndQuadrant-cross.png"))

(defun tapoueh-expert-postgresql-logo ()
  "Get the :style-sheet property and rework the link to the CSS"
  '(:img :src "/static/expert-postgresql.png"))

(defun tapoueh-current-page-url ()
  "Get the current page full URL"
  (format nil "http://~a~a" *host* *script-name*))

(defun tapoueh-extract-directive (directive-name pathname)
  (when pathname
    (slot-value (muse-parse-directives pathname)
		(intern (string-upcase directive-name) :tapoueh))))

(defun muse-current-file ()
  (and (muse-p *muse-current-file*)	; could be nil
       (muse-pathname *muse-current-file*)))

(defun article-list-position (document)
  "Find the position of given pathname in *blog-articles-list*"
  (position (namestring (muse-pathname document))
	    (mapcar #'namestring *blog-articles-list*)
	    :test #'string=))

(defun get-navigation-link (link title
			    &key
			      (class "previous")
			      (title-format "« ~a"))
  (when link
    `(:a :class ,class :href ,link
	 ,(format nil title-format title))))

(defun tapoueh-list-parent-directory (script-name)
  (let ((current-source-dir
	 (directory-namestring (muse-source script-name))))
    (values
     current-source-dir
     (remove-if-not #'fad:directory-pathname-p
		    (fad:list-directory
		     (fad:pathname-parent-directory current-source-dir))))))

(defun tapoueh-previous-directory (&optional (script-name *script-name*))
  "Provide a link to the previous directory"
  (multiple-value-bind (current-source-dir entries)
      (tapoueh-list-parent-directory script-name)
    (when (let* ((path-list (split-pathname current-source-dir))
		 (pos       (position "blog" path-list :test #'string=)))
	    ;; only consider blog/ sub-tree
	    (and pos (not (= (+ 1 pos) (length path-list)))))
      (loop
	 for (a b . c) on entries
	 when (and a b (fad:pathname-equal current-source-dir b))
	 return (format nil "/~a" (relative-pathname-from *root-directory* a))))))

(defun tapoueh-next-directory (&optional (script-name *script-name*))
  "Provide a link to the previous directory"
  (multiple-value-bind (current-source-dir entries)
      (tapoueh-list-parent-directory script-name)
    (when (let* ((path-list (split-pathname current-source-dir))
		 (pos       (position "blog" path-list :test #'string=)))
	    ;; only consider blog/ sub-tree
	    (and pos (not (= (+ 1 pos) (length path-list)))))
      (loop
	 for (a b . c) on entries
	 when (and a b (fad:pathname-equal current-source-dir a))
	 return (format nil "/~a" (relative-pathname-from *root-directory* b))))))

(defun tapoueh-insert-recent-articles-title ()
  "Conditionnaly include a title"
  (when (and (muse-p *muse-current-file*)
	     (muse-article-p *muse-current-file*))
    `(:h3 :class "prevnext" "Recent Articles")))

(defun tapoueh-insert-previous-article ()
  "Provide a link to the previous article"
  (if (and (muse-p *muse-current-file*)
	   (muse-article-p *muse-current-file*))
      (let* ((pos  (article-list-position *muse-current-file*))
	     (path (when (< 0 pos) (nth (- pos 1) *blog-articles-list*)))
	     (prev (when path (gethash path *blog-articles*))))
	(when prev
	  (get-navigation-link (muse-url prev) (muse-title prev)
			       :class "previous" :title-format "« ~a")))
      ;; not an article
      (let* ((prev  (tapoueh-previous-directory))
	     (title (when prev (first (last (split-pathname prev))))))
	(get-navigation-link prev title
			     :class "previous" :title-format "« ~a"))))

(defun tapoueh-insert-next-article ()
  "Provide a link to the previous article"
  (if (and (muse-p *muse-current-file*)
	   (muse-article-p *muse-current-file*))
    (let* ((pos  (article-list-position *muse-current-file*))
	   (path (nth (+ pos 1) *blog-articles-list*))
	   (prev (when path (gethash path *blog-articles*))))
      (when prev
	(get-navigation-link (muse-url prev) (muse-title prev)
			     :class "next pull-right" :title-format "~a »")))
    ;; not an article
    (let* ((next  (tapoueh-next-directory))
	   (title (when next (first (last (split-pathname next))))))
      (get-navigation-link next title
			   :class "next pull-right" :title-format "~a »"))))

(defun tapoueh-insert-breadcrumb-here (&optional (*script-name* *script-name*))
  "path from root to current page"
  (let* ((blog-url-p  (url-within-p "/blog/" :script-name *script-name*))
	 (muse-url-p  (and (muse-p *muse-current-file*)
			   (muse-article-p *muse-current-file*)))
	 (dirs        (split-pathname *script-name*))
	 (dirs        (mapcar #'fad:pathname-as-directory
			      (if muse-url-p (butlast dirs) dirs))))
    (when (or muse-url-p blog-url-p)
      `(:ul :class "breadcrumb"
	    (:li (:span :class "divider" (:i :class "icon-sitemap")))
	    ,@(loop
		 for (d . more?) on (cons "/dev/dim" dirs)
		 for cur = *root-directory* then (expand-file-name-into d cur)
		 for rel = "" then (relative-pathname-from *root-directory* cur)
		 collect `(:li (:a :href ,(concatenate 'string "/" rel)
				   ,(namestring (fad:pathname-as-file d)))
			       ,(when more?
				      '(:span :class "divider" "/"))))))))

(defun tapoueh-insert-article-date-here ()
  (when (and (muse-p *muse-current-file*)
	     (muse-date *muse-current-file*))
    `(:div :class "date"
	   ,(muse-format-date *muse-current-file*)
	   " "
	   (:i :class "icon-calendar"))))

(defun tapoueh-social-div ())

(defun tapoueh-insert-latest-articles
    (&optional n
       (base-directory
	(when (muse-p *muse-current-file*)
	  (fad:pathname-as-file
	   (directory-namestring (muse-pathname *muse-current-file*))))))
  "Insert the N latest articles found under BASE-DIRECTORY."
  (let* ((articles (find-blog-articles base-directory))
	 (n (or n 5))
	 (l (length articles)))
    ;; return the N first articles
    (format-article-list (subseq (reverse articles) 0 (min n l)))))

(defun tapoueh-list-blog-articles (&optional subdirs-only no-index root)
  "Run through all subdirs from current page and list pages"
  nil)

(defun tags-cloud ()
  "Return a tags cloud data structure suitable for JQCloud"
  (let ((counts (make-hash-table :test #'equal)))
    (reduce (lambda (&rest tags-args)
	      (loop
		 for tags in tags-args
		 do (loop
		       for tag in tags
		       do (incf (gethash tag counts 0)))))
	    (mapcar #'muse-tags
		    (alexandria:hash-table-values *blog-articles*)))
    (loop
       for (tag . count)
       in (sort (alexandria:hash-table-alist counts) #'> :key #'cdr)
       collect (list (cons :text tag)
		     (cons :weight count)
		     (cons :link (format nil "/tags/~a" (string-downcase tag)))))))

(defun tapoueh-insert-article-tags ()
  "Return tags for the current article."
  (when (and (muse-p *muse-current-file*)
	     (muse-date *muse-current-file*))
    `(:div :style "text-align: right;"
	   (:span ,@(muse-format-tags *muse-current-file*)) " "
	   (:i :class "icon-tags"))))

(defun tapoueh-insert-article-tag-image-here ()
  "Return the main tag image for the current article"
  (when (and (muse-p *muse-current-file*)
	     (muse-date *muse-current-file*))
    (let* ((image (muse-extract-article-image-source *muse-current-file*))
	   (ifile (third image))
	   (tag   (string-downcase
		   (car (rassoc ifile *article-default-image-for-tag*
				:test #'string=)))))
      `(:div :class "span2 pull-left"
	     (:a :class "thumbnail" :href ,(format nil "/tags/~a" tag)
		 (:img :class "img-polaroid"
		       :style "width: 160px; height: 120px;"
		       :src ,(if (listp image) (third image) image)))))))

(defun tapoueh-insert-social-div-here ()
  "Return the <div> code for the Tweet this and G+1 elements"
  (when (and (muse-p *muse-current-file*)
	     (muse-date *muse-current-file*))
    `(:div :id "social" :class "span1 pull-right social"
	   (:ul
	    (:li (:|g:plusone|
		   :size "tall"
		   :href ,(muse-url *muse-current-file* :with-base-url t)))
	    (:li (:a :href "http://twitter.com/share"
		     :class "twitter-share-button"
		     :data-via "tapoueh"
		     :data-count "vertical"
		     "Tweet")
		 (:script :type "text/javascript"
			  :src "http://platform.twitter.com/widgets.js"))))))x
