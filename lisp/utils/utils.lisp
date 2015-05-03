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

(defun muse-encode-wierd-date-string (month-name day-cruft year-string)
  "We've been using strange date formats, e.g. \"July 3rd, 2010\"."
  (let* ((months '("January" "February" "March"
		   "April" "May" "June"
		   "July" "August" "September"
		   "October" "November" "December"))
	 (month  (+ 1 (position month-name months :test #'equal)))
	 (day    (parse-integer day-cruft :junk-allowed t))
	 (year   (parse-integer year-string)))
    (muse-encode-timestamp :year year :month month :day day)))

(defun parse-date (date-string)
  "Parse the data of a Muse document"
  (cond
    ;; We don't always have been using numbers to represent dates...
    ((not (digit-char-p (aref date-string 0)))
     (ppcre:register-groups-bind (month day year)
	 ("(\\w+) ([^,]+), (\\d+)" "July 3rd, 2010")
       (muse-encode-wierd-date-string month day year)))

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
(defun expand-file-name-into (pathname directory &key name type)
  "Expand pathname into given directory. If the PATHNAME directory is
   absolute, make it relative first so that we ensure we always get back a
   pathname under DIRECTORY."
  (when pathname
    (let* ((dir (string-trim '(#\/) (directory-namestring pathname)))
	   (p   (make-pathname :directory `(:relative ,dir)
			       :name (or name (pathname-name pathname))
			       :type (or type (pathname-type pathname)))))
      (merge-pathnames p directory))))

;;
;; File dependency prevent us from putting that in url.lisp because we want
;; to be able to use it both from ssi.lisp and web.lisp
;;
(defun url-within-p (prefix &key request script-name)
  "Returns non-nil if the script-name begins with PREFIX."
  (let* ((script-name (or script-name
			  (hunchentoot:script-name* request)))
	 (len         (length prefix)))
    (and script-name
	 (<= len (length script-name))
	 (string= prefix (subseq script-name 0 len)))))

;;;
;;; Timing Macro
;;;
(defun elapsed-time-since (start)
  "Return how many seconds ticked between START and now"
  (/ (- (get-internal-real-time) start)
     internal-time-units-per-second))

(defmacro timing (&body forms)
  "return both how much real time was spend in body and its result"
  (let ((start (gensym))
	(end (gensym))
	(result (gensym)))
    `(let* ((,start (get-internal-real-time))
	    (,result (progn ,@forms))
	    (,end (get-internal-real-time)))
       (values
	,result
	(float (/ (- ,end ,start) internal-time-units-per-second))))))

(defun replace-symbols-recursively (code symbols)
  "Walk CODE to replace symbols as given in the SYMBOLS alist."
  (loop
     for s-exp in code
     when (listp s-exp) collect (replace-symbols-recursively s-exp symbols)
     else collect (if (symbolp s-exp)
		      (destructuring-bind (before . after)
			  (or (assoc s-exp symbols) (cons nil nil))
			(if before after s-exp))
		      s-exp)))

(defmacro displaying-time ((fmt &rest bindings) &body forms)
  "display on *standard-output* how much time it took to execute body"
  (let* ((result   (gensym))
	 (timing   (gensym))
	 (bindings
	  (replace-symbols-recursively bindings `((result . ,result)
						  (timing . ,timing)))))
    `(multiple-value-bind (,result ,timing)
	 (timing ,@forms)
       (format t ,fmt ,@bindings)
       ,result)))

;;;
;;; Others
;;;
(defun google-maps-url-for (search-terms)
  "Return a suitable URL for the Google Maps service"
  (let ((query (cl-ppcre:regex-replace-all " " search-terms "+")))
   (format nil "http://maps.google.com/maps?q=~a&z=5" query)))


;;;
;;; Images and thumbnails
;;;
(defun thumbnail (script-name &optional (dir "thumbnails"))
  "Prepare a thumbnail for SCRIPT-NAME, an image URL that begins with /images/"
  (let* ((components (split-sequence:split-sequence #\/ script-name))
         (thumbnail  (format nil "~{/~a~}" (append (list dir)
                                                   (cddr components))))
         (thumbdir   (format nil "~a~{~a~^/~}/"
                             *html-directory*
                             (append (list dir) (butlast (cddr components)))))
         (source     (namestring
                      (merge-pathnames (subseq script-name 1) *root-directory*)))
         (target     (namestring
                      (merge-pathnames (subseq thumbnail 1) *html-directory*)))
         (command    (format nil "convert -geometry 160x120 ~s ~s" source target)))
    (ensure-directories-exist thumbdir)
    (when (or (not (probe-file target))
              (< (file-write-date target)
                 (file-write-date source)))
      (uiop:run-program command))
    thumbnail))
