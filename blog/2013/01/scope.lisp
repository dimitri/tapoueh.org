;;; See https://my.smeuh.org/al/blog/lost-in-scope

(defpackage #:lost-in-scope
  (:use #:cl)
  (:export #:any-day-but-monday?))

(in-package #:lost-in-scope)

(defparameter *days*
  '(monday tuesday wednesday thursday friday saturday sunday)
  "List of days in the week")

(defun any-day-but-monday? (day)
  "Returns a generalized boolean, true unless DAY is 'monday"
  (member day (remove-if (lambda (day) (eq day 'monday)) *days*)))

(defparameter *callbacks-all-sunday*
    (loop
       for day in *days*
       collect (lambda () day))
  "loop binds DAY only once")

(defparameter *callbacks*
  (mapcar (lambda (day)
	    ;; for each day, produce a separate closure
	    ;; around its own lexical variable day
	    (lambda () day))
	  *days*)
  "A list of callbacks to return the current day...")
