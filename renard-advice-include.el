;;; renard-advice-include.el --- Seb Gross
;;
;; advice <include> so that:
;;  - it figures out lang on its own
;;  - it copies the source file in the muse output path
;;

(defadvice muse-publish-include-tag (around cw:muse-html-insert-contents activate)
  "Add some stuff around <include> tag."
  (let
      ((file (muse-publish-get-and-delete-attr "file" attrs))
       (markup (muse-publish-get-and-delete-attr "markup" attrs)))

    (unless file
      (error "No file attribute specified in <include> tag"))
    
    (if (string-equal markup "src")
	(progn
	  ;; set up attributes for muse-publish-include-tag
	  (setq attrs (aput 'attrs "file" file))
	  (setq attrs (aput 'attrs "markup" "src"))
	  (setq attrs (aput 'attrs "lang"
			    (substring (format "%s"
					       (with-temp-buffer
						 (insert-file-contents file)
						 (set-auto-mode)
						 major-mode)) 0 -5)))

	  ;; insert include header
	  (muse-insert-markup
	   (replace-regexp-in-string
	    "%s" file cw:muse-publish-src-include-header))

	  ;; publish the file content as an <include> tag
	  (ad-set-arg 0 (point))
	  (ad-set-arg 1 (point))
	  ad-do-it

	  ;; insert include footer
	  (muse-insert-markup
	   (replace-regexp-in-string
	    "%s" file cw:muse-publish-src-include-footer))

	  ;; copy the file
	  (copy-file
	   (expand-file-name
	    file (file-name-directory muse-publishing-current-file))
	   (expand-file-name
	    file (file-name-directory muse-publishing-current-output-path))
	   t t t))

      ;; else, default call
      ad-do-it)))

(provide 'renard-advice-include)
