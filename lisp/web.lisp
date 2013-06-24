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

(defun html-script-name (request)
  "Arrange our muse handler to be called for any URI"
  (declare (ignore request))
  t)

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
					      (not (muse-index-p p))))
		  :with-images nil)
		 (ssi-file *footer*))))

(defun 404-page ()
  "Return a 404 error code."
  (setf (hunchentoot:return-code*) hunchentoot:+HTTP-NOT-FOUND+))

(hunchentoot:define-easy-handler (muse :uri #'html-script-name) ()
  "Catch-all handler, do the routing ourselves."
  (let* ((script-name (hunchentoot:script-name*))
	 (muse-source (muse-source script-name))
	 *muse-current-file*)
    (declare (special *muse-current-file*))

    (cond
      ((null muse-source)         (404-page))
      ;; ((muse-index-p muse-source) (render-index-page muse-source))
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
