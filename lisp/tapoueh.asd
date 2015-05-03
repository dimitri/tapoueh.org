;;;; lisp.asd

(asdf:defsystem #:tapoueh
    :serial t
    :description "Publishing System for http://tapoueh.org"
    :author "Dimitri Fontaine"
    :license "WFTPL"
    :depends-on (#:esrap               ; parser
		 #:string-case         ; string-case macro
		 #:local-time          ; handle time based computing
		 #:cl-who              ; produce HTML from Muse sources
		 #:split-sequence      ; easy splitting
		 #:fiveam              ; regression testing
		 #:cl-fad              ; file and directories
		 #:cl-ppcre            ; regular expressions
		 #:hunchentoot         ; web server
		 #:alexandria          ; tools such as hash-table-keys, etc
		 #:cl-json             ; to output json to the web controler
		 #:uiop                ; quit
		 #:command-line-arguments
		 )
    ;; do NOT include tapoueh.lisp here, as it begins with #!, not lispy
    :components
    ((:module "lib"
              :components
              ((:file "simple-routes")))
     (:file "package" :depends-on ("lib"))
     (:module "utils"
              :depends-on ("package")
              :components
              ((:file "cli-parser")
               (:file "utils")))
     (:module "muse"
              :depends-on ("package" "utils")
              :components
              ((:file "muse")
               (:file "muse-parser" :depends-on ("muse"))))
     (:module "src"
              :depends-on ("package" "lib" "utils" "muse")
              :components
              ((:file "collection")
               (:file "ssi"          :depends-on ("collection"))
               (:file "render-pages" :depends-on ("collection" "ssi" "url"))
               (:file "compiler"     :depends-on ("collection" "render-pages" "ssi"))
               (:file "url"          :depends-on ("ssi" "collection"))
               (:file "web"          :depends-on ("collection" "ssi" "url"))))))

