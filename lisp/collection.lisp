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
    (setf (gethash pathname *blog-articles*) article)

    (when re-sort-list (compute-sorted-blog-articles-list))

    ;; return the article we just added, as a muse structure
    article))

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
