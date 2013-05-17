;;;; url.lisp
;;;
;;; The lisp software publishing http://tapoueh.org
;;

(in-package #:tapoueh)

(def-suite url :description "Emacs Muse Parser Test Suite.")
(in-suite url)

;; Routing helpers, figuring out what document to serve given the script
;; name in the URI.
;;
(defun pathname-from-script-name-as-directory
    (script-name &aux (local-name (subseq script-name 1)))
  "Given a script-name, return the directory where to find the resource"
  (fad:pathname-as-directory (merge-pathnames local-name *root-directory*)))

(defun resource-name (script-name)
  "Return pathname of resource we should be serving, by installing the
   filename and type defaults into the script-name from the URI.

   SCRIPT-NAME must be a relative filename, not beginning with /."
  (cond
    ((null (pathname-name script-name))
     ;; /blog/ --> /blog/index.html
     (make-pathname :directory script-name
		    :name      *default-file-name*
		    :type      *default-file-type*))
    ((null (pathname-type script-name))
     (if (fad:directory-exists-p
	  (pathname-from-script-name-as-directory script-name))
	 ;; /blog --> /blog/index.html
	 (make-pathname :directory (namestring (fad:pathname-as-file script-name))
			:name      *default-file-name*
			:type      *default-file-type*)
	 ;; /blog/2013/05/13-from-parser-to-compiler --> add .html
	 (let* ((pathname  (pathname script-name))
		(directory (directory-namestring pathname))
		(as-file   (if (string= "/" directory) directory
			       (fad:pathname-as-file directory))))
	   (make-pathname :directory (namestring as-file)
			  :name      (file-namestring pathname)
			  :type      *default-file-type*))))
    (t
     (pathname script-name))))

#+5am
(test resource-name
      "Test the transformation of an URI script-name into a resource-name"
      (is (string= (namestring (resource-name "/blog/")) "//blog//index.html"))
      (is (string= (namestring (resource-name "/blog"))  "//blog/index.html"))
      (is (string= (namestring (resource-name "/foo"))   "///foo.html"))
      (is (string=
	   (namestring (resource-name "/blog/2013/05/13-from-parser-to-compiler"))
	   "//blog/2013/05/13-from-parser-to-compiler.html")))

(defun muse-source (script-name)
  "Return the .muse pathname to use to serve given pathname, or nil"
  (let* ((resource-name (resource-name script-name)))
    (expand-file-name-into resource-name *root-directory*
			   :type *muse-pathname-type*)))

#+5am
(test muse-source
      "Test the script-name URI parsing"
      (is (fad:pathname-equal
	   (muse-source "/blog")
	   (expand-file-name-into (make-pathname :directory "blog"
						 :name "index"
						 :type "muse")
				  *root-directory*)))
      (is (fad:pathname-equal
	   (muse-source "/blog/")
	   (expand-file-name-into (make-pathname :directory "blog"
						 :name "index"
						 :type "muse")
				  *root-directory*)))
      (is (fad:pathname-equal
	   (muse-source "/blog/index")
	   (expand-file-name-into (make-pathname :directory "blog"
						 :name "index"
						 :type "muse")
				  *root-directory*)))
      (is (fad:pathname-equal
	   (muse-source "/blog/2013/05/13-from-parser-to-compiler")
	   (expand-file-name-into (make-pathname :directory "/blog/2013/05"
						 :name "13-from-parser-to-compiler"
						 :type "muse")
				  *root-directory*)))
      (is (fad:pathname-equal
	   (muse-source "/blog/2013/03/18-bulk-replication.html")
	   (expand-file-name-into (make-pathname :directory "/blog/2013/03"
						 :name "18-bulk-replication"
						 :type "muse")
				  *root-directory*))))

;;
;; From a pathname to an URL
;;
(defun split-pathname (pathname)
  "Returns a list of pathname elements: directories then filename then type."
  (remove-if
   (lambda (x) (string= x ""))		; leading and trailing slashes
   (split-sequence:split-sequence #\/ (namestring pathname))))

#+5am
(test split-pathname
      "Test the pathname splitting function"
      (is (equalp (split-pathname "/Users/dim/dev/tapoueh.org/")
		  '("Users" "dim" "dev" "tapoueh.org")))
      (is (equalp
	   (split-pathname
	    #P"/Users/dim/dev/tapoueh.org/blog/2008/12/04-fake-entry.muse")
	   '("Users" "dim" "dev" "tapoueh.org" "blog" "2008" "12" "04-fake-entry.muse"))))

(defun relative-pathname-from (root pathname)
  "Return the relative pathname for PATHNAME within ROOT directory"
  (if (fad:pathname-relative-p pathname)
      (expand-file-name-into pathname root)

      ;; now working with an absolute pathname
      (loop
	 for r in (append (split-pathname root) '("")) ; ensure one more loop
	 for (p . rest) on (split-pathname pathname)   ; so that we get the rest
	 when (string/= r p)
	 return (format nil "~{~a~^/~}" (cons p rest)))))

#+5am
(test relative-pathname
      "Test the relative pathname function"
      (is (equalp
	   (relative-pathname-from "/Users/dim/dev/tapoueh.org/"
				   "/Users/dim/dev/tapoueh.org/blog/2008/12/04-fake-entry.muse")
	   "blog/2008/12/04-fake-entry.muse")))

(defmethod muse-url ((m muse))
  "Returns the full URL to get at given Muse document"
  (let ((script-name
	 (relative-pathname-from *root-directory* (muse-pathname m))))
    (concatenate 'string
		 ;; *base-url*
		 "/"
		 ;; we chop off the pathname-type here
		 (directory-namestring script-name)
		 (pathname-name script-name))))

#+5am
(test muse-url
      (is (equalp
	   (muse-url (muse-parse-directives "/Users/dim/dev/tapoueh.org/blog/2013/05/13-from-parser-to-compiler.muse"))
	   "http://tapoueh.org/blog/2013/05/13-from-parser-to-compiler")))
