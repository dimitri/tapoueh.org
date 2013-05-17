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
  (find-all-blog-articles)		; set our sorted list of articles

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
