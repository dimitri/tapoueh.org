;;;; web.lisp
;;;
;;; The lisp software publishing http://tapoueh.org

(in-package #:tapoueh)

(defparameter *articles-per-index* 5
  "How many articles fit in a default index page")

(defparameter *default-file-name* "index")
(defparameter *default-file-type* "html")

(defparameter *port* 8042)
(defparameter *access-log-file* "/tmp/tapoueh-access.log")
(defparameter *message-log-file* "/tmp/tapoueh.log")

(defvar *acceptor* nil "The web server")

;;
;; Render documents, with header and footer
;;
(defun render-muse-document (&key article
			       (*script-name* (or *script-name*
						  (muse-url article))))
  "Parse the muse document at PATHNAME and spits out HTML"
  (let* ((pathname            (if article (muse-pathname article)
				  (muse-source *script-name*)))
	 (*muse-current-file* (or article
				  (muse-parse-article pathname))))
    (concatenate 'string
		 (ssi-file *header*)
		 (to-html *muse-current-file*)
		 (ssi-file *footer*))))

(defun render-index-page (&optional (*script-name* *script-name*) article-list)
  "Produce a listing of articles for a given index location"
  (let* ((pathname            (muse-source *script-name*)))
    (concatenate 'string
		 (ssi-file *header*)
		 (article-list-to-html-with-chapeau
		  (or article-list
		      (find-blog-articles (directory-namestring pathname))))
		 (ssi-file *footer*))))

(defun render-reversed-index-page (&optional
				     (*script-name* *script-name*)
				     article-list
				     (n *articles-per-index*))
  "Produce the main blog article listing page."
  (concatenate 'string
	       (ssi-file *header*)
	       (article-list-to-html-with-chapeau
		(reverse (last (or article-list
				   (find-blog-articles *blog-directory*)) n)))
	       (ssi-file *footer*)))

(defun render-tag-cloud ()
  "Produce our tags cloud"
  (json:encode-json-to-string (tags-cloud)))

(defun render-tag-listing (&optional (*script-name* *script-name*) article-list)
  "Produce a listing of articles for given tag"
  (let ((tag-name (second (split-pathname *script-name*))))
    (concatenate 'string
		 (ssi-file *header*)
		 (article-list-to-html-with-chapeau
		  (or article-list
		      (find-blog-articles-with-tag *blog-directory* tag-name)))
		 (ssi-file *footer*))))

(defun render-rss-feed (&optional (*script-name* *script-name*) article-list)
  "Produce the RSS feed for the given tag, or all articles"
  ;; filter out the type (.xml) for backward compatibility
  (let ((tag-name (pathname-name (second (split-pathname *script-name*)))))
    (article-list-to-rss
     (reverse
      (or article-list
	  ;; the RSS stream should contain the full article
	  (find-blog-articles-with-tag *blog-directory* tag-name
				       :parse-fn #'muse-parse-article))))))

;;
;; Routing proper
;;
(loop
   for (prefix path) in '(("/images/"    "../images/")
			  ("/static/"    "../static/")
			  ("/resources/" "../resources/"))
   do (push (hunchentoot:create-folder-dispatcher-and-handler
	     prefix
	     (asdf:system-relative-pathname :tapoueh path))
	    hunchentoot:*dispatch-table*))

(defun tag-url-p (request)
  "Return non-nil when request's script-name is within /tags/"
  (url-within-p "/tags/" :request request))

(defun blog-article-p (request)
  "Return True only when request's script-name is within /blog/"
  (url-within-p "/blog/" :request request))

(defun rss-url-p (request)
  "Return non-nil when request's script-name is within /rss/"
  (url-within-p "/rss/" :request request))

(defun pathname-is-blog-index-p (pathname)
  "Return non-nil when PATHNAME is a blog index page"
  (and
   ;; blog indexes are dynamically generated content
   ;; in other parts of the website, indexes are documents
   (member "blog" (split-pathname pathname) :test #'string=)
   (string= (pathname-name pathname) "index")))

(defun muse-document-p (request)
  "Return non-nil when requested to serve an existing .muse file"
  (declare (ignore request))		; hunchentoot API
  (let ((pathname (muse-source (hunchentoot:script-name*))))
    (and pathname
	 (not (pathname-is-blog-index-p pathname))
	 (probe-file pathname))))

(defun blog-index-p (request)
  "Return non-nil when DOCUMENT is an Index page."
  (declare (ignore request))		; hunchentoot API
  (let ((pathname (muse-source (hunchentoot:script-name*))))
    (pathname-is-blog-index-p pathname)))

(defun 404-page ()
  "Return a 404 error code."
  (setf (hunchentoot:return-code*) hunchentoot:+HTTP-NOT-FOUND+))

(hunchentoot:define-easy-handler (document :uri #'muse-document-p) ()
  "Render a Muse document that we have the source of."
  (let ((*script-name* (hunchentoot:script-name*))
	(*host*        (hunchentoot:host)))
    (render-muse-document)))

(hunchentoot:define-easy-handler (blog-index :uri #'blog-index-p) ()
  "Render an Index Page at the given location."
  (let* ((*script-name* (hunchentoot:script-name*))
	 (*host*        (hunchentoot:host)))
    (render-index-page)))

(hunchentoot:define-easy-handler (home :uri "/") ()
  "Let's design an home page..."
  (let ((*script-name* (hunchentoot:script-name*))
	(*host*        (hunchentoot:host)))
    (render-reversed-index-page "/blog/")))

(hunchentoot:define-easy-handler (blog :uri "/blog") ()
  "The blog home page is all dynamic, not based on a Muse file."
  ;; XXX: that could be a very simple SSI Muse document?
  (let ((*script-name* (hunchentoot:script-name*))
	(*host*        (hunchentoot:host)))
    (render-reversed-index-page "/blog/")))

(hunchentoot:define-easy-handler (cloud :uri "/cloud.json") ()
  "A tags Cloud, the JSON data"
  (setf (hunchentoot:content-type*) "text/plain")
  (render-tag-cloud))

(hunchentoot:define-easy-handler (rss :uri #'rss-url-p) ()
  "Render a RSS content for articles tagged with given tag"
  (setf (hunchentoot:content-type*) "application/rss+xml")
  (let* ((*script-name* (hunchentoot:script-name*))
	 (*host*        (hunchentoot:host)))
    (render-rss-feed)))

(hunchentoot:define-easy-handler (tags :uri #'tag-url-p) ()
  "Render a list of articles tagged with given tag"
  (let* ((*script-name* (hunchentoot:script-name*))
	 (*host*        (hunchentoot:host)))
    (render-tag-listing)))

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
