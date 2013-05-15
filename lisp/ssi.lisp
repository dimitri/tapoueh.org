;;;; ssi-lisp.lisp
;;;
;;; The lisp software publishing http://tapoueh.org
;;;
;;; Here we read HTML files and replace <lisp> tags with the result of
;;; evaluating the code they contain.

(in-package #:tapoueh)

(defparameter *base-url* "http://tapoueh.org/"
  "Only used to publish in the headers Meta.")

(defvar *muse-current-file* nil
  "Maintained in the web.lisp hunchentoot handlers.")

(def-suite ssi :description "HTML SSI Parser Test Suite.")
(in-suite ssi)

;;
;; we use cl-pprce:regexp-replace facilities, in particular the replacement
;; function in simple-calls mode.
;;
(defun replace-<lisp> (match &rest registers)
  "Eval a <lisp>(code)</lisp> code block, returns its result as a string."
  (declare (ignore match))
  (format nil "~a" (eval (let ((*package* (find-package 'tapoueh)))
			   (read-from-string (first registers))))))

(defun eval-lisp-tags (string)
  "Replace each <lisp>(code)</lisp> by the result of evaluating the code."
  (ppcre:regex-replace-all "<lisp>(.*)</lisp>" string #'replace-<lisp>
			   :simple-calls t))

#+5am
(test replace-ssi-lisp-tags
      "Test parsing a Server Side Include: html with <lisp>(code)</lisp>"
      (is (string= (eval-lisp-tags "<lisp>(+ 1 2)</lisp>") "3"))
      (is (string= (eval-lisp-tags "<html><lisp>(+ 1 2)</lisp></html>")
		   "<html>3</html>")))

(defun ssi-file (pathname)
  "Server Side Include file at given PATHNAME, replacing <lisp> tags"
  (eval-lisp-tags (slurp-file-into-string pathname)))

;;;
;;; Helper functions used in the SSI directives
;;;
;;; Compatibility layer kept to be able to publish the website in a
;;; subdirectory somewhere, for now just spit back a / based URL.
;;;
(defun tapoueh-style-sheet ()
  "Return the link to the main CSS"
  "<link rel=\"stylesheet\" type=\"text/css\"  media=\"all\" href=\"/static/styles.css\" />")

(defun tapoueh-rss-index ()
  "Get the relative link to the RSS feed"
  "/rss/tapoueh.xml")

(defun tapoueh-contents ()
  "Output the link to the /contents.html page"
  "/contents.html")

(defun tapoueh-root-index ()
  "Output the link to the /contents.html page"
  "/index.html")

(defun tapoueh-2ndquadrant-logo ()
  "Get the :style-sheet property and rework the link to the CSS"
  "<img src=\"/static/2ndQuadrant-cross.png\">")

(defun tapoueh-expert-postgresql-logo ()
  "Get the :style-sheet property and rework the link to the CSS"
  "<img src=\"/static/expert-postgresql.png\">")

(defun tapoueh-current-page-url ()
  "Get the current page full URL"
  (format nil "http://~a~a"
	  (hunchentoot:host)
	  (hunchentoot:script-name*)))

(defun tapoueh-extract-directive (directive-name pathname)
  (when pathname
    (slot-value (parse-muse-directives pathname)
		(intern (string-upcase directive-name) :tapoueh))))

(defun muse-current-file ()
  (and (muse-p *muse-current-file*)	; could be nil
       (muse-pathname *muse-current-file*)))

(defun tapoueh-insert-previous-article ())
(defun tapoueh-insert-next-article ())
(defun tapoueh-insert-breadcrumb-here ())
(defun tapoueh-insert-article-date-here ()
  (when (muse-p *muse-current-file*)
    (let ((stamp (muse-timestamp *muse-current-file*)))
      (local-time:format-timestring
       nil stamp
       :format
       (append '(:long-weekday ", " :long-month " " :day " " :year)
	       (unless (and (= 0 (local-time:timestamp-hour stamp))
			    (= 0 (local-time:timestamp-minute stamp)))
		 '(", " :hour ":" :min)))))))
(defun tapoueh-social-div ())
