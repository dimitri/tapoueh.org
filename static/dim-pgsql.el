;;; dim-pgsql.el --- Dimitri Fontaine
;;

;; PostgreSQL source code
(add-hook 'c-mode-hook
	  (function
	   (lambda nil 
	     (when (and buffer-file-name
			(or (string-match "pgsql" buffer-file-name)
			    (string-match "pgext" buffer-file-name)
			    (string-match "postgresql" buffer-file-name)))
	       (c-set-style "bsd")
	       (setq c-basic-offset 4) 
	       (setq tab-width 4)
	       (c-set-offset 'case-label '+)
	       (setq indent-tabs-mode t)))))

;;; To work on the documentation, the following (or a variant, as above)
;;; can be helpful.

(defun pgsql-sgml-mode ()
  "SGML mode adjusted for PostgreSQL project"
  (interactive)
  (sgml-mode)
  (setq indent-tabs-mode nil)
  (setq sgml-basic-offset 1))

(setq auto-mode-alist
  (cons '("\\(postgres\\|pgsql\\).*\\.sgml\\'" . pgsql-sgml-mode)
        auto-mode-alist))

;;;
;;; Implement a linum-mode hook to display PostgreSQL functions line numbers
;;;
(require 'linum)

(defun dim:pgsql-current-func ()
  "return first line number of current function, if any"
  (save-excursion
    (let* ((start (point))
	   (prev-create-function 
	    (re-search-backward "create.*function" nil t))
	   (open-as-$$ 
	    (when prev-create-function
	      ;; limit the search to next semi-colon
	      (let ((next-semi-col (re-search-forward ";" nil t)))
		(goto-char prev-create-function)
		(re-search-forward "AS.*\\$\\([^$\n]*\\)\\$" next-semi-col t))))
	   ($$-name
	    (when open-as-$$ (match-string-no-properties 1)))
	   ($$-line-num
	    (when open-as-$$ (line-number-at-pos)))
	   (begin-line-num
	    (when open-as-$$
	      (unless (looking-at "\n") (forward-char))
	      (if (string-match "begin" (current-word))
		  (1- (line-number-at-pos))
		(line-number-at-pos))))
	   (close-as-$$ 
	    (when open-as-$$
	      (re-search-forward (format "\\$%s\\$" $$-name) nil t)
	      (beginning-of-line)
	      (point)))
	   (reading-function
	    (when (and open-as-$$ close-as-$$)
	      (and (or (>= start open-as-$$)
		       (and (not (eq $$-line-num begin-line-num))
			    (= (line-number-at-pos start) $$-line-num)))
		   (< start close-as-$$)))))

      (if reading-function begin-line-num nil))))

(defun dim:pgsql-linum-format (line)
  "Return the current line number linum output"
  (if (not (equal major-mode 'sql-mode))
      (format "%S" line)
    (save-excursion
      ;; (goto-line line)
      (goto-char (point-min)) (forward-line (1- line))
      (let ((current-func-start (dim:pgsql-current-func)))
	(if current-func-start
	    (format "%3d %5d" (- line current-func-start) line)
	  (format "%9d" line))))))

(setq linum-format 'dim:pgsql-linum-format)

(provide 'dim-pgsql)
