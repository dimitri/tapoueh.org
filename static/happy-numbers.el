;;; happy-numbers.el --- Dimitri Fontaine
;;
;; http://programmingpraxis.com/2010/07/23/happy-numbers/
;;
(require 'cl) ; subseq

(defun happy? (&optional n seen)
  "return true when n is a happy number"
  (interactive)
  (let* ((number    (or n (read-from-minibuffer "Is this number happy: ")))
	 (digits    (mapcar 'string-to-int (subseq (split-string number "") 1 -1)))
	 (squares   (mapcar (lambda (x) (* x x)) digits))
	 (happiness (apply '+ squares)))
    (cond ((eq 1 happiness)      t)
	  ((memq happiness seen) nil)
	  (t                     (happy? (number-to-string happiness)
					 (push happiness seen))))))

(defun find-happy-numbers (&optional limit)
  "find all happy numbers from 1 to limit"
  (interactive)
  (let ((count (or limit (read-from-minibuffer "List of happy numbers from 1 to: ")))
	happy)
    (dotimes (n (string-to-int count))
      (when (happy? (number-to-string (1+ n)))
	(push (1+ n) happy)))
    (nreverse happy)))

(provide 'happy-numbers)

