;;;; web.lisp
;;;
;;; The lisp software publishing http://tapoueh.org

(in-package #:tapoueh)

(defvar *acceptor* nil "The web server")
(defvar *server-is-running* nil)

(defvar *routes*
  (compile-routes
   (:GET    ""                                      'serve-reversed-index-page)
   (:GET    "/"                                     'serve-reversed-index-page)
   (:GET    "/blog"                                 'serve-reversed-index-page)
   (:GET    "/blog/index.html"                      'serve-reversed-index-page)

   (:GET    "/blog/:year"                           'serve-index-page)
   (:GET    "/blog/:year/:month"                    'serve-index-page)
   (:GET    "/blog/:year/:month/:filename"          'serve-muse-document)
   (:GET    "/blog/archives.html"                   'serve-blog-archives)
   (:GET    "/rss/:tag"                             'serve-rss-feed)
   (:GET    "/tags/:tag"                            'serve-tag-listing)

   (:GET    "/about"                                'serve-muse-document)
   (:GET    "/about.html"                           'serve-muse-document)
   (:GET    "/pgsql/.*"                             'serve-muse-document)
   (:GET    "/emacs/.*"                             'serve-muse-document)

   (:GET    "/cloud.json"                           'serve-tag-cloud)

   (:GET    "/projects"                             'serve-projects-index-page)
   (:GET    "/projects.html"                        'serve-projects-index-page)
   (:GET    "/conferences"                          'serve-confs-index-page)
   (:GET    "/conferences.html"                     'serve-confs-index-page)
   (:GET    "/confs/:year/:month/:filename"         'serve-muse-document)

   (:GET    "/thumbnails/.*"                        'serve-thumbnail)

   (:GET    "/status"                               'serve-server-status)))


;;;
;;; Routines used to serve pages
;;;
;;; TODO: use the arguments given by the simple-route URL matching above
;;; rather than keep the old code portions figuring it out again from the
;;; (hunchentoot:script-name*)
;;;
(defun serve-reversed-index-page ()
  (let ((*script-name* (hunchentoot:script-name*))
	(*host*        (hunchentoot:host)))
    (render-reversed-index-page "/blog/")))

(defun serve-index-page (&optional year month)
  (declare (ignore year month))
  (let* ((*script-name* (hunchentoot:script-name*))
         (*host*        (hunchentoot:host)))
    (render-index-page)))

(defun serve-muse-document (&optional year month filename)
  (declare (ignore year month filename))
  (let ((*script-name* (hunchentoot:script-name*))
	(*host*        (hunchentoot:host)))
    (render-muse-document)))

(defun serve-rss-feed (tag)
  (declare (ignore tag))
  (setf (hunchentoot:content-type*) "application/rss+xml")
  (let* ((*script-name* (hunchentoot:script-name*))
	 (*host*        (hunchentoot:host)))
    (with-output-to-string (s)
      (render-rss-feed s))))

(defun serve-tag-listing (tag)
  (declare (ignore tag))
  (let* ((*script-name* (hunchentoot:script-name*))
         (*host*        (hunchentoot:host)))
    (render-tag-listing)))

(defun serve-blog-archives ()
  (let* ((*script-name* (hunchentoot:script-name*))
         (*host*        (hunchentoot:host))
         (articles      (find-blog-articles *blog-directory*))
         (len           (length articles)))
    (render-reversed-index-page "/blog/" articles len)))

(defun serve-projects-index-page ()
  (let ((*script-name* (hunchentoot:script-name*))
	(*host*        (hunchentoot:host)))
    (render-projects-index-page
     "/projects"
     (find-muse-documents :base-directory *pgsql-directory*
			  :parse-fn #'muse-parse-chapeau)
     (find-muse-documents :base-directory *emacs-directory*
			  :parse-fn #'muse-parse-chapeau))))

(defun serve-confs-index-page ()
  (let ((*script-name* (hunchentoot:script-name*))
	(*host*        (hunchentoot:host)))
    (render-confs-index-page
     "/conferences"
     (find-muse-documents :base-directory *confs-directory*
			  :parse-fn #'muse-parse-chapeau))))

(defun serve-tag-cloud ()
  (setf (hunchentoot:content-type*) "text/plain")
  (render-tag-cloud))

(defun serve-thumbnail ()
  "Allow serving from the *HTML-DIRECTORY* when using the dynamic app."
  (let* ((components (split-sequence:split-sequence #\/ (hunchentoot:script-name*)))
         (pathname (format nil "~a/~a/~{~a~^/~}"
                           *html-directory*
                           "thumbnails"
                           (cddr components))))
    (hunchentoot:handle-static-file pathname)))

(defun start-web-server (&key port root logs)
  "Start the hunchentoot web server."
  (read-config)
  (find-all-blog-articles)		; set our sorted list of articles

  (setf *acceptor*
        (make-instance 'simpleroutes-acceptor
                       :routes '*routes*
                       :port (or port *port*)
                       :document-root (or root *root-directory*)
                       :access-log-destination (or logs *access-log-file*)
                       :message-log-destination (or logs *logfile*)))

  (setf *server-is-running* t)
  (hunchentoot:start *acceptor*))

(defun stop-web-server ()
  "Stop the hunchentoot server"
  (hunchentoot:stop *acceptor*)
  (setf *acceptor* nil *server-is-running* nil))

(defun restart-web-server ()
  "Stop then start the web server, in case the acceptor properties need to
   be changed."
  (stop-web-server)
  (start-web-server))

(defun serve-server-status ()
  "Return OK when the server is OK."
  (setf (hunchentoot:content-type*) "text/plain")
  "OK")
