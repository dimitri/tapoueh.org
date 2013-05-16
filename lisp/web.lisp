;;;; web.lisp
;;;
;;; The lisp software publishing http://tapoueh.org

(in-package #:tapoueh)

(def-suite web :description "Emacs Muse Parser Test Suite.")
(in-suite web)

(defparameter *root-directory*
  (asdf:system-relative-pathname :tapoueh "../"))

(defparameter *blog-directory*
  (asdf:system-relative-pathname :tapoueh "../blog/"))

(defparameter *header*
  (asdf:system-relative-pathname :tapoueh "../static/header.html"))

(defparameter *footer*
  (asdf:system-relative-pathname :tapoueh "../static/footer.html"))

(defparameter *default-file-name* "index")
(defparameter *default-file-type* "html")

(defparameter *port* 8042)
(defparameter *access-log-file* "/tmp/tapoueh-access.log")
(defparameter *message-log-file* "/tmp/tapoueh.log")

(defvar *acceptor* nil "The web server")

;;
;; Render documents, with header and footer
;;
(defun render-muse-document (pathname)
  "Parse the muse document at PATHNAME and spits out HTML"
  (let ((*muse-current-file* (parse-muse-article pathname)))
    (declare (special *muse-current-file*))
    (concatenate 'string
		 (ssi-file *header*)
		 (to-html *muse-current-file*)
		 (ssi-file *footer*))))

;;
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
;; Routing proper
;;
(loop
   for (prefix path) in '(("/images/"    "../images/")
			  ("/css/"       "../css/")
			  ("/static/"    "../static/")
			  ("/resources/" "../resources/"))
   do (push (hunchentoot:create-folder-dispatcher-and-handler
	     prefix
	     (asdf:system-relative-pathname :tapoueh path))
	    hunchentoot:*dispatch-table*))

(defun html-script-name (request)
  "Arrange our muse handler to be called for any URI"
  (declare (ignore request))
  t)

(hunchentoot:define-easy-handler (muse :uri #'html-script-name) ()
  "Catch-all handler, do the routing ourselves."
  (let* ((script-name (hunchentoot:script-name*))
	 (muse-source (muse-source script-name))
	 *muse-current-file*)
    (declare (special *muse-current-file*))
    (if (and muse-source (probe-file muse-source))
	(progn
	  (hunchentoot:log-message* :INFO "rendering file '~a'" muse-source)
	  (render-muse-document muse-source))

	;; FIXME: we also serve RSS (.xml) here
	(setf (hunchentoot:return-code*) hunchentoot:+HTTP-NOT-FOUND+))))

(defun start-web-server (&key
			   (port *port*)
			   (access-log *access-log-file*)
			   (message-log *message-log-file*))
  "Start the hunchentoot web server."
  ;; didn't see how to change the existing acceptor's port...
  (setf *acceptor*
	(make-instance 'hunchentoot:easy-acceptor
		       :document-root "/tmp"
		       :port port
		       :access-log-destination access-log
		       :message-log-destination message-log))
  (hunchentoot:start *acceptor*))

(defun stop-web-server ()
  "Stop the hunchentoot server"
  (hunchentoot:stop *acceptor*))

(defun restart-web-server ()
  "Stop then start the web server, in case the acceptor properties need to
   be changed."
  (stop-web-server)
  (start-web-server))
