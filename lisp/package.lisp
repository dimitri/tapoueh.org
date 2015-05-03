;;;; package.lisp

(defpackage #:tapoueh
  (:use #:cl #:esrap #:simple-routes)
  (:import-from #:string-case
		#:string-case)
  (:import-from #:cl-who
		#:with-html-output
		#:with-html-output-to-string
		#:htm
		#:str
		#:esc
		#:fmt)
  (:import-from #:fiveam
		#:test
		#:run!
		#:is
		#:in-suite
		#:def-suite)
  (:import-from #:split-sequence
		#:split-sequence)
  (:export #:*html-directory*
           #:*root-directory*
           #:start-web-server
           #:stop-web-server
           #:restart-web-server
           #:compile-articles))

