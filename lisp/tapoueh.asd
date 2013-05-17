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
		 #:cl-ppcre		; regular expressions
		 #:hunchentoot		; web server
		 #:alexandria		; tools such as hash-table-keys, etc
		 )
    :components ((:file "package")
		 (:file "utils"       :depends-on ("package"))
		 (:file "muse"        :depends-on ("utils" "package"))
		 (:file "collection"  :depends-on ("utils" "package" "muse"))
		 (:file "ssi"         :depends-on ("package" "collection"))
		 (:file "muse-parser" :depends-on ("muse" "ssi"))
		 (:file "url"         :depends-on ("muse" "ssi" "collection"))
		 (:file "web"         :depends-on ("utils"
						   "package"
						   "collection"
						   "ssi"
						   "url"
						   "muse"))
		 (:file "tapoueh"     :depends-on ("package" "web"))))

