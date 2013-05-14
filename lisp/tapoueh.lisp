;;;; tapoueh.lisp
;;;
;;; The lisp software publishing http://tapoueh.org

(in-package #:tapoueh)

(defparameter *root-directory*
  (asdf:system-relative-pathname :tapoueh "../"))

(defparameter *blog-directory*
  (asdf:system-relative-pathname :tapoueh "../blog/"))

(defun find-all-blog-articles (&key
				 sort
				 (parsefn #'parse-muse-directives))
  "Find all .muse articles from *blog-directory*"
  (let ((articles '()))
    (fad:walk-directory *blog-directory*
			(lambda (p)
			  (let ((m (funcall parsefn p)))
			    (when (and (muse-p m)
				       (muse-article-p m))
			      (push m articles))))
			:test #'muse-file-type-p)

    ;; sort the list, maybe
    (if sort (sort articles #'sort-articles) articles)))
