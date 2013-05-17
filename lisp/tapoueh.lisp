#! /usr/bin/sbcl --script
;;; tapoueh.lisp
;;;
;;; The lisp software publishing http://tapoueh.org

;;; load the necessary components then parse the command line
;;; and launch the work

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; now is the time to load our Quicklisp project
(format t "Loading quicklisp and the :tapoueh project and its dependencies...")
(terpri)
(with-output-to-string (*standard-output*)
  (ql:quickload '(:swank :tapoueh)))

(in-package #:tapoueh)

(defparameter *opt-spec*
  `((("help" #\h) :type boolean :documentation "show usage")

    (("doc-root" #\d) :type string :initial-value "/tmp"
     :documentation "Document Root")

    (("port" #\p) :type integer :initial-value 8042
     :documentation "web server port")

    (("swank-port" #\P) :type integer :initial-value 4205
     :documentation "Emacs control port for M-x slime-connect")

    ("log" :type string :initial-value "/tmp/tapoueh.log"
	   :documentation "main log file")

    ("access-log" :type string :initial-value "/tmp/tapoueh.access.log"
		  :documentation "log file for http access logs")))

(defun main (argv)
  "Entry point when building an executable image with buildapp"
  (multiple-value-bind (options arguments)
      (command-line-arguments:process-command-line-options *opt-spec* argv)
    (declare (ignore arguments))
    (destructuring-bind
	  (&key help port swank-port log access-log doc-root)
	options

      (when help
	(command-line-arguments:show-option-help *opt-spec*)
	(uiop:quit))

      ;; start a swank server to take interactive control if needs be
      (swank:create-server :port swank-port :style :spawn :dont-close t)

      ;; and start the web server itself
      (start-web-server :document-root doc-root
			:port port
			:access-log access-log
			:message-log log)
      (read))))

;;; actually call the main function, too
(main (uiop:command-line-arguments))

