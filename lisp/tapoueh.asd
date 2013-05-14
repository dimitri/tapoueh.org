;;;; lisp.asd

(asdf:defsystem #:tapoueh
  :serial t
  :description "Publishing System for http://tapoueh.org"
  :author "Dimitri Fontaine"
  :license "WFTPL"
  :depends-on (#:esrap			; parser
	       #:string-case		; string-case macro
	       #:local-time		; handle time based computing
	       #:cl-who			; produce HTML from Muse sources
	       #:split-sequence		; easy splitting
	       #:fiveam			; regression testing
	       #:cl-fad			; file and directories
	       )
  :components ((:file "package")
	       (:file "utils"   :depends-on ("package"))
               (:file "muse"    :depends-on ("utils" "package"))
	       (:file "tapoueh" :depends-on ("muse" "utils" "package"))))

