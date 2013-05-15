;;;; utils.lisp
;;;
;;; Some basic utilities we will need

(in-package #:tapoueh)

;;
;; File utils
;;
(defun slurp-file-into-string (filename)
  "Return given filename's whole content as a string."
  (with-open-file (stream filename
			  :direction :input
			  :external-format :utf-8)
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
(defvar *new-time-format* '((:year  0 4)
			    (:month 4 6)
			    (:day   6 8)
			    (:hour  9 11)
			    (:mins  12 14))
  "Meta data needed to parse date string such as 20130513-11:08")

(defvar *old-time-format* '((:year  0 4)
			    (:month 4 6)
			    (:day   6 8))
  "Meta data needed to parse date string such as 20081204")

(defun muse-encode-timestamp (&key year month day
				(hour 0) (mins 0)
				(secs 0) (nsecs 0))
  "We don't expect SECS and NSECS in the date format used in our Muse articles."
  (local-time:encode-timestamp nsecs secs mins hour day month year))

(defun muse-parse-date-string (date-string &key (format *new-time-format*))
  "Parse given DATE-STRING according to FORMAT."
  (apply #'muse-encode-timestamp
	 (loop
	    for (key start end) in format
	    append (list key
			 (parse-integer (subseq date-string start end))))))

(defun parse-date (date-string)
  "Parse the data of a Muse document"
  (cond
    ;; Mainly the new format is used: 20130513-11:08
    ((= 14 (length date-string))
     (muse-parse-date-string date-string :format *new-time-format*))

    ;; Some articles are using the format: 20081204
    ((= 8 (length date-string))
     (muse-parse-date-string date-string :format *old-time-format*))

    (t
     (error "Don't know how to parse date: \"~a\"." date-string))))

;;
;; pathname routines
;;
(defun expand-file-name-into (pathname directory &key type)
  "Expand pathname into given directory. If the PATHNAME directory is
   absolute, make it relative first so that we ensure we always get back a
   pathname under DIRECTORY."
  (let* ((dir (string-trim '(#\/) (directory-namestring pathname)))
	 (p   (make-pathname :directory `(:relative ,dir)
			     :name (pathname-name pathname)
			     :type (or type (pathname-type pathname)))))
    (merge-pathnames p directory)))
