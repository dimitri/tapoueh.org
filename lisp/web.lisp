;;;; web.lisp
;;;
;;; The lisp software publishing http://tapoueh.org

(in-package #:tapoueh)

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
  (hunchentoot:log-message* :INFO "rendering file '~a'" pathname)
  (let ((*muse-current-file* (muse-parse-article pathname)))
    (declare (special *muse-current-file*))
    (concatenate 'string
		 (ssi-file *header*)
		 (to-html *muse-current-file*)
		 (ssi-file *footer*))))

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

(defun tag-url-p (request)
  "Return non-nil when request's script-name is within /tags/"
  (let ((script-name (hunchentoot:script-name* request)))
    (and script-name
	 (<= 6 (length script-name))
	 (string= "/tags/" (subseq script-name 0 6)))))

(defun blog-article-p (request)
  "Return True only when request's script-name is within /blog/"
  (let ((script-name (hunchentoot:script-name* request)))
    (and script-name
	 (<= 6 (length script-name))
	 (string= "/blog/" (subseq script-name 0 6)))))

(defun muse-document-p (request)
  "For the non-blog parts of the website"
  (not (blog-article-p request)))

(defun muse-index-p (pathname)
  "Return a generalized boolean true when DOCUMENT is an Index page."
  (and pathname (string= (pathname-name pathname) "index")))

(defun render-index-page (pathname)
  "Render an index page, which is a listing of articles"
  (let ((*muse-current-file* (muse-parse-article pathname)))
    (declare (special *muse-current-file*))
    (concatenate 'string
		 (ssi-file *header*)
		 (article-list-to-html
		  (find-blog-articles (directory-namestring pathname)
				      :test (lambda (p)
					      (not (muse-index-p p)))))
		 (ssi-file *footer*))))

(defun 404-page ()
  "Return a 404 error code."
  (setf (hunchentoot:return-code*) hunchentoot:+HTTP-NOT-FOUND+))

(hunchentoot:define-easy-handler (home :uri "/") ()
  "Let's design an home page"
  (render-muse-document (muse-source "/")))

(hunchentoot:define-easy-handler (about :uri "/about") ()
  "Let's design an home page"
  (render-muse-document (muse-source "/")))

(hunchentoot:define-easy-handler (projects :uri "/projects") ()
  "Let's design a projects page"
  (render-muse-document (muse-source "/projects")))

(hunchentoot:define-easy-handler (confs :uri "/confs") ()
  "Let's design a conferences page"
  (render-muse-document (muse-source "/conferences")))

(hunchentoot:define-easy-handler (cloud :uri "/cloud") ()
  "A tags Cloud, the JSON data"
  (setf (hunchentoot:content-type*) "text/plain")
  (json:encode-json-to-string (tags-cloud)))

(hunchentoot:define-easy-handler (tags :uri #'tag-url-p) ()
  "Catch-all handler, do the routing ourselves."
  (let* ((script-name (hunchentoot:script-name*))
	 (tag-name    (second (split-pathname script-name))))
    (concatenate 'string
		 (ssi-file *header*)
		 (article-list-to-html
		  (find-blog-articles-with-tag *root-directory* tag-name))
		 (ssi-file *footer*))))

(hunchentoot:define-easy-handler (blog :uri #'blog-article-p) ()
  "Catch-all handler, do the routing ourselves."
  (let* ((script-name (hunchentoot:script-name*))
	 (muse-source (muse-source script-name))
	 *muse-current-file*)
    (declare (special *muse-current-file*))

    (cond
      ((null muse-source)         (404-page))
      ((muse-index-p muse-source) (render-index-page muse-source))
      ((probe-file muse-source)	  (render-muse-document muse-source))
      (t                          (404-page)))))

(hunchentoot:define-easy-handler (muse :uri #'muse-document-p) ()
  "Catch-all handler, do the routing ourselves."
  (let* ((script-name (hunchentoot:script-name*))
	 (muse-source (muse-source script-name))
	 *muse-current-file*)
    (declare (special *muse-current-file*))

    (cond
      ((null muse-source)         (404-page))
      ((probe-file muse-source)	  (render-muse-document muse-source))
      (t                          (404-page)))))

(defun start-web-server (&key
			   (document-root "/tmp")
			   (port *port*)
			   (access-log *access-log-file*)
			   (message-log *message-log-file*))
  "Start the hunchentoot web server."
  (find-all-blog-articles)		; set our sorted list of articles

  (setf *acceptor*
	(make-instance 'hunchentoot:easy-acceptor
		       :document-root document-root
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
