;;; tapoueh-migrate.el --- take old articles in a single Muse file and split them
;;

(defun tapoueh-migrate-articles (from to)
  "Migrate articles found in the FROM muse file to the TO directory"
  (with-temp-buffer
    (insert-file-contents-literally from)
    (beginning-of-buffer)
    (while (re-search-forward "^* " nil t)
      (let* ((date
	      (buffer-substring-no-properties (point)
					      (1- (re-search-forward " " nil t))))
	     (title
	      (buffer-substring-no-properties (point)
					      (muse-line-end-position)))
	     (dummy (progn
		      (forward-line)
		      (re-search-forward (rx bol word) nil t)
		      (beginning-of-line)))
	     (content
	      (buffer-substring-no-properties
	       (point)
	       (or (cond ((re-search-forward "^* " nil t)
			  (forward-line -1)
			  (point))

			 ((re-search-forward "^<include " nil t)
			  (forward-line -1)
			  (point))

			 (t (point-max))))))

	     (year   (substring date 0 4))
	     (month  (substring date 4 6))
	     (day    (substring date 6 8))
	     (dirs   (concat to "/" year "/" month))
	     (fname  (downcase
		      (replace-regexp-in-string
		       "[.!,'?&:=~/()]" "" (replace-regexp-in-string " " "-" title))))
	     (file   (concat dirs "/" day "-" fname ".muse")))

	(message "%S %S" file title)))))

	;; (unless (file-directory-p dirs)
	;;   (make-directory dirs 'parents)
	;;   (with-temp-file (concat dirs "/index.muse")
	;;     (insert (format "#title Archives %s %s\n\n" year month)
	;; 	    (format "** =ls -l /dev/dim/blog/%s/%s=\n\n" year month)
	;; 	    "<lisp>(tapoueh-insert-latest-articles)</lisp>\n")))

	;; (with-temp-file file
	;;   (insert "#author Dimitri Fontaine\n"
	;; 	  (format "#title  %s\n" (replace-regexp-in-string "=" "" title))
	;; 	  (format "#date   %s\n" date)
	;; 	  (format "#tags   %s\n"
	;; 		  (mapconcat 'identity
	;; 			     (tapoueh-get-tag-list content) " "))
	;; 	  "\n"
	;; 	  (format "* %s\n" title)
	;; 	  "\n"
	;; 	  (replace-regexp-in-string
	;; 	   (concat (regexp-quote "[[../")
	;; 		   (rx (or "images" "static")))
	;; 		   "[[../../../images/" content)))))))

(loop with dest = "~/dev/new.tapoueh.org/blog"
      for muse in '("~/dev/tapoueh.org/blog/blog.dim.muse")
		    ;; "~/dev/tapoueh.org/blog/blog.dim.2009.muse"
		    ;; "~/dev/tapoueh.org/blog/news.dim.muse")
      do (tapoueh-migrate-articles muse dest))

(defun tapoueh-guess-new-link ()
  "Try to guess the new link from the old anchor"
  (interactive)
  ;; from this: #%20Introducing%20Extensions
  ;; find that: blog/2010/10/21-introducing-extensions.html
  (let* ((anchor (buffer-substring-no-properties (region-beginning)
						 (region-end)))
	 (link
	  (loop for (src link title date fdate tags)
		in (tapoueh-walk-articles "~/dev/new.tapoueh.org/blog/")
		when (string-match
		      (replace-regexp-in-string
		       "%[0-9A-F][0-9A-F]" ".*"
		       (replace-regexp-in-string "#" "" anchor)) title)
		return (concat "blog/" link))))
    (forward-line 1)
    (backward-char 1)
    (insert (format " %s" link))))

