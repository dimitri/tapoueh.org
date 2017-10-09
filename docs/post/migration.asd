;;;; pginstall.asd

(asdf:defsystem #:migration
    :serial t
    :description "Migration Muse content to Markdown"
    :author "Dimitri Fontaine <dim@tapoueh.org>"
    :license "WTFPL"
    :version "0.0.1"
    :depends-on (#:uiop			; host system integration
		 #:drakma		; http client, download archives
                 #:cl-ppcre             ; Regular Expressions
                 #:split-sequence       ; split sequences
                 #:md5                  ; check archive checksums
                 #:alexandria           ; some utils
                 #:tapoueh              ; Muse document parser
                 #:puri                 ; parse URLs
                 #:trivia               ; CL pattern matching
                 )
    :components ((:file "migration")))

