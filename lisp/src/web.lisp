;;;; web.lisp
;;;
;;; The lisp software publishing http://tapoueh.org

(in-package #:tapoueh)

(defparameter *port* 8042)
(defparameter *access-log-file* "/tmp/tapoueh-access.log")
(defparameter *message-log-file* "/tmp/tapoueh.log")

(defvar *acceptor* nil "The web server")

(defvar *routes*
  (compile-routes
   (:GET    ""                                      'serve-reversed-index-page)
   (:GET    "/"                                     'serve-reversed-index-page)
   (:GET    "/blog"                                 'serve-reversed-index-page)

   (:GET    "/blog/:year"                           'serve-index-page)
   (:GET    "/blog/:year/:month"                    'serve-index-page)
   (:GET    "/blog/:year/:month/:filename"          'serve-muse-document)
   (:GET    "/blog/archives.html"                   'serve-blog-archives)
   (:GET    "/rss/:tag"                             'serve-rss-feed)
   (:GET    "/tags/:tag"                            'serve-tag-listing)

   (:GET    "/about"                                'serve-muse-document)
   (:GET    "/pgsql/.*"                             'serve-muse-document)
   (:GET    "/emacs/.*"                             'serve-muse-document)

   (:GET    "/cloud.json"                           'serve-tag-cloud)

   (:GET    "/projects"                             'serve-projects-index-page)
   (:GET    "/conferences"                          'serve-confs-index-page)
   (:GET    "/confs/:year/:month/:filename"         'serve-muse-document)


   ;; (:GET    "/images/*"                             'serve-static-resource)
   ;; (:GET    "/thumbnails/*"                         'serve-static-resource)
   ;; (:GET    "/static/*"                             'serve-static-resource)
   ;; (:GET    "/resources/*"                          'serve-static-resource)
   ))


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

(defun start-web-server (&key
			   (document-root *root-directory*)
			   (port *port*)
			   (access-log *access-log-file*)
			   (message-log *message-log-file*))
  "Start the hunchentoot web server."
  (find-all-blog-articles)		; set our sorted list of articles

  (setf *acceptor* (make-instance 'simpleroutes-acceptor
                                  :routes '*routes*
                                  :port port
                                  :document-root document-root
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
