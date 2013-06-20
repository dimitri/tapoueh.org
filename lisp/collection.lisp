;;;; collection.lisp
;;;
;;; Maintain a list of all articles in this blog
;;;

(in-package #:tapoueh)

(defparameter *root-directory*
  (asdf:system-relative-pathname :tapoueh "../"))

(defparameter *blog-directory*
  (asdf:system-relative-pathname :tapoueh "../blog/"))

(defparameter *header*
  (asdf:system-relative-pathname :tapoueh "../static/header.html"))

(defparameter *footer*
  (asdf:system-relative-pathname :tapoueh "../static/footer.html"))

(defvar *blog-articles* (make-hash-table :test 'equal :size 1024)
  "Hash-table of all blog articles, keyed by their pathname.")

(defvar *blog-articles-list* nil
  "List of blog articles PATHNAMEs, ordered by article date in
   find-all-blog-articles, not maintained by add-article. Not costly enough
   to maintain to bother, as of now.")

(defun compute-sorted-blog-articles-list ()
  "Set *blog-articles-list* as the sorted-by-timestamp list of
  *blog-articles* documents pathnames."
  (let ((articles (alexandria:hash-table-values *blog-articles*)))
    (setq *blog-articles-list*
	  (mapcar #'muse-pathname
		  (sort (remove-if (lambda (x) (null (muse-date x))) articles)
			#'sort-articles)))))

(defun add-article (pathname &key re-sort-list)
  "Add article found at PATHNAME into *blog-articles*.
   When RE-SORT-LIST is true, then we generate a whole new
   *blog-articles-list* version.

   Return the parsed article."
  (let ((article (muse-parse-directives pathname)))
    (when (and (muse-p article) (muse-article-p article))
      (setf (gethash pathname *blog-articles*) article)

      (when re-sort-list (compute-sorted-blog-articles-list))

      ;; return the article we just added, as a muse structure
      article)))

(defun find-all-blog-articles (&key purge)
  "Find all .muse articles from *blog-directory*, parse them and keep them
     in a hash-table where the pathname is the key."
  (when purge
    (setq *blog-articles* (make-hash-table :test 'equal :size 1024)))
  (fad:walk-directory *root-directory* #'add-article :test #'muse-file-type-p)
  (compute-sorted-blog-articles-list))

(defun maybe-parse-document (pathname)
  "Lookup PATHNAME *blog-articles*, if not found, fetch from disk and add it."
  (let ((article (gethash pathname *blog-articles*)))
    (if (or (null article) (file-newer-p article))
	(add-article pathname :re-sort-list t)
	article)))

(defun find-blog-articles (base-directory &key test)
  "Find all muse articles in given BASE-DIRECTORY, return a sorted list of
   them. Don't even have a look at files when TEST returns true, if provided."
  (let (articles)
    (fad:walk-directory (if (fad:pathname-absolute-p base-directory)
			    (fad:pathname-directory-pathname base-directory)
			    (expand-file-name-into base-directory
						   *root-directory*))
			(lambda (pathname)
			  (when (or (null test)
				    (funcall test pathname))
			   (let ((doc (muse-parse-chapeau pathname)))
			     (when (muse-article-p doc)
			       (push doc articles)))))
			:test #'muse-file-type-p)
    ;; sort the articles now
    (sort articles #'muse-article-before-p)))

(defun format-article-list (list &key with-images)
  "Given a LIST of muse articles (proper muse structure), return the cl-who
   forms needed to render the list to html"
  `(:ul ,@(when with-images '(:class "images"))
    ,@(loop
	 for article in list
	 collect (muse-format-article article :with-image with-images))))

(defun article-list-to-html (list &key with-images)
  "Produce the HTML Listing of the given list of articles."
  (concatenate 'string
	       (eval `(with-html-output-to-string (s)
			,(format-article-list list :with-images with-images)))))
