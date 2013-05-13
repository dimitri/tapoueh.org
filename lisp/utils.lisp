;;;; utils.lisp
;;;
;;; Some basic utilities we will need

(in-package #:tapoueh)

;;
;; File utils
;;
(defun slurp-file-into-string (filename)
  "Return given filename's whole content as a string."
  (with-open-file (stream filename :direction :input)
    (let ((seq (make-array (file-length stream)
			   :element-type 'character
			   :fill-pointer t)))
      ;; apparently the fastest way at that is read-sequence
      ;; http://www.ymeme.com/slurping-a-file-common-lisp-83.html
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

;;
;; Parsing time formats found in our Muse article collection
;;
(defvar *time-format* '((:year  0 4)
			(:month 4 6)
			(:day   6 8)
			(:hour  9 11)
			(:mins  12 14))
  "Meta data needed to parse date string such as 20130513-11:08")

(defun muse-encode-timestamp (&key year month day hour mins (secs 0) (nsecs 0))
  "We don't expect SECS and NSECS in the date format used in our Muse articles."
  (local-time:encode-timestamp nsecs secs mins hour day month year))

(defun parse-date (date-string)
  "Parse the data of a Muse document"
  (cond
    ((= 14 (length date-string))
     (apply #'muse-encode-timestamp
	    (loop
	       for (key start end) in *time-format*
	       append (list key
			    (parse-integer (subseq date-string start end))))))
    (t
     (error "Don't know how to parse date: \"~a\"." date-string))))

