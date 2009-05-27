;;; dim-muse-hack.el --- Emacs Muse Hack to have better publishing date
;;
;; we change only the datestamp part from the original code

;; adapt the "default" muse rss header settings

(setq muse-journal-heading-regexp
      "\\(?:\\([0-9:-]+\\)\\(?:: \\)?\\)?\\(.+?\\)?")

(setq muse-journal-html-heading-regexp
      (concat "^<h2[^>\n]*>" muse-journal-heading-regexp "</h2>$"))

(setq muse-journal-rss-heading-regexp
      (concat "^\\* " muse-journal-heading-regexp "$"))

(defun muse-journal-html-munge-buffer ()
  (goto-char (point-min))
  (let ((heading-regexp muse-journal-html-heading-regexp)
        (inhibit-read-only t))
    (while (re-search-forward heading-regexp nil t)
      (let* ((date (match-string 1))
             (orig-date date)
             (title (match-string 2))
             (clean-title title)
             datestamp qotd text)
        (delete-region (match-beginning 0) (match-end 0))
        (if clean-title
            (save-match-data
              (while (string-match "\\(^<[^>]+>\\|<[^>]+>$\\)" clean-title)
                (setq clean-title (replace-match "" nil nil clean-title)))))

        (save-match-data
          (cond ((and date
                     (string-match
                      (concat "\\`\\([1-9][0-9][0-9][0-9]\\)[./]?"
                              "\\([0-1][0-9]\\)[./]?\\([0-3][0-9]\\)"
			      "[-:]?\\([0-2][0-9]\\)[:]?\\([0-5][0-9]\\)") date))

		 (setq date
		       (encode-time
			0
			(string-to-number (match-string 5 date))
			(string-to-number (match-string 4 date))
			(string-to-number (match-string 3 date))
			(string-to-number (match-string 2 date))
			(string-to-number (match-string 1 date))
			(current-time-zone))

		       ;; make sure that date is in a format that RSS
		       ;; readers can handle
		       date (let ((system-time-locale "C"))
			      (format-time-string
			       (muse-style-element :date-format) date))))

		((and date
                     (string-match
                      (concat "\\`\\([1-9][0-9][0-9][0-9]\\)[./]?"
                              "\\([0-1][0-9]\\)[./]?\\([0-3][0-9]\\)") date))

		 (setq date
		       (encode-time
			0 0 0
			(string-to-number (match-string 3 date))
			(string-to-number (match-string 2 date))
			(string-to-number (match-string 1 date))
			(current-time-zone))

		       date (let ((system-time-locale "C"))
			      (format-time-string
			       (muse-style-element :date-format-notime) date))))))

        (save-restriction
          (narrow-to-region
           (point) (if (re-search-forward
                        (concat "\\(^<hr>$\\|"
                                heading-regexp "\\)") nil t)
                       (match-beginning 0)
                     (point-max)))
          (goto-char (point-max))
          (while (and (not (bobp))
                      (eq ?\  (char-syntax (char-before))))
            (delete-char -1))
          (goto-char (point-min))
          (while (and (not (eobp))
                      (eq ?\  (char-syntax (char-after))))
            (delete-char 1))
          (save-excursion
            (when (search-forward "<qotd>" nil t)
              (let ((tag-beg (match-beginning 0))
                    (beg (match-end 0))
                    end)
                (re-search-forward "</qotd>\n*")
                (setq end (point-marker))
                (save-restriction
                  (narrow-to-region beg (match-beginning 0))
                  (muse-publish-escape-specials (point-min) (point-max)
                                                nil 'document)
                  (setq qotd (buffer-substring-no-properties
                              (point-min) (point-max))))
                (delete-region tag-beg end)
                (set-marker end nil))))
          (setq text (buffer-string))
          (delete-region (point-min) (point-max))
          (let ((entry muse-journal-html-entry-template))
            (muse-insert-file-or-string entry)
            (muse-publish-mark-read-only (point-min) (point-max))
            (goto-char (point-min))
            (while (search-forward "%date%" nil t)
              (remove-text-properties (match-beginning 0) (match-end 0)
                                      '(read-only nil rear-nonsticky nil))
              (replace-match (or date "") nil t))
            (goto-char (point-min))
            (while (search-forward "%title%" nil t)
              (remove-text-properties (match-beginning 0) (match-end 0)
                                      '(read-only nil rear-nonsticky nil))
              (replace-match (or title "&nbsp;") nil t))
            (goto-char (point-min))
            (while (search-forward "%anchor%" nil t)
              (replace-match (muse-journal-anchorize-title
                              (or clean-title orig-date))
                             nil t))
            (goto-char (point-min))
            (while (search-forward "%qotd%" nil t)
              (save-restriction
                (narrow-to-region (match-beginning 0) (match-end 0))
                (delete-region (point-min) (point-max))
                (when qotd (muse-insert-markup qotd))))
            (goto-char (point-min))
            (while (search-forward "%text%" nil t)
              (remove-text-properties (match-beginning 0) (match-end 0)
                                      '(read-only nil rear-nonsticky nil))
              (replace-match text nil t))
            (when (null qotd)
              (goto-char (point-min))
              (when (search-forward "<div class=\"entry-qotd\">" nil t)
                (let ((beg (match-beginning 0)))
                  (re-search-forward "</div>\n*" nil t)
                  (delete-region beg (point))))))))))
  ;; indicate that we are to continue the :before-end processing
  nil)


(defun muse-journal-rss-munge-buffer ()
  (goto-char (point-min))
  (let ((heading-regexp muse-journal-rss-heading-regexp)
        (inhibit-read-only t))
    (while (re-search-forward heading-regexp nil t)
      (let* ((date (match-string 1))
             (orig-date date)
             (title (match-string 2))
             ;; FIXME: Nothing is done with qotd
             enclosure qotd desc)
        (if title
            (save-match-data
              (if (string-match muse-explicit-link-regexp title)
                  (setq enclosure (muse-get-link title)
                        title (muse-get-link-desc title)))))
        (save-match-data
          (cond ((and date
                     (string-match
                      (concat "\\([1-9][0-9][0-9][0-9]\\)[./]?"
                              "\\([0-1][0-9]\\)[./]?\\([0-3][0-9]\\)"
			      "[-:]?\\([0-2][0-9]\\)[:]?\\([0-5][0-9]\\)") date))
		 (setq date
		       (encode-time
			0
			(string-to-number (match-string 5 date))
			(string-to-number (match-string 4 date))
			(string-to-number (match-string 3 date))
			(string-to-number (match-string 2 date))
			(string-to-number (match-string 1 date))
			(current-time-zone))

		       ;; make sure that date is in a format that RSS
		       ;; readers can handle
		       date (let ((system-time-locale "C"))
			      (format-time-string
			       (muse-style-element :date-format) date))))

		;; default case
		((and date
		      (string-match
		       (concat "\\([1-9][0-9][0-9][0-9]\\)[./]?"
			       "\\([0-1][0-9]\\)[./]?\\([0-3][0-9]\\)") date))
		 (setq date
		       (encode-time
			0 0 0
			(string-to-number (match-string 3 date))
			(string-to-number (match-string 2 date))
			(string-to-number (match-string 1 date))
			(current-time-zone))

		       ;; make sure that date is in a format that RSS
		       ;; readers can handle
		       date (let ((system-time-locale "C"))
			      (format-time-string
			       (muse-style-element :date-format) date))))))
		
        (save-restriction
          (narrow-to-region
           (match-beginning 0)
           (if (re-search-forward heading-regexp nil t)
               (match-beginning 0)
             (if (re-search-forward "^Footnotes:" nil t)
                 (match-beginning 0)
               (point-max))))
          (goto-char (point-min))
          (delete-region (point) (muse-line-end-position))
          (re-search-forward "</qotd>\n+" nil t)
          (while (and (char-after)
                      (eq ?\  (char-syntax (char-after))))
            (delete-char 1))
          (let ((beg (point)))
            (if (muse-style-element :summarize)
                (progn
                  (forward-sentence 2)
                  (setq desc (concat (buffer-substring beg (point)) "...")))
              (save-restriction
                (muse-publish-markup-buffer "rss-entry" "journal-rss-entry")
                (goto-char (point-min))
                (if (re-search-forward "Page published by Emacs Muse" nil t)
                    (goto-char (muse-line-end-position))
                  (muse-display-warning
                   (concat
                    "Cannot find 'Page published by Emacs Muse begins here'.\n"
                    "You will probably need this text in your header."))
                  (goto-char (point-min)))
                (setq beg (point))
                (if (re-search-forward "Page published by Emacs Muse" nil t)
                    (goto-char (muse-line-beginning-position))
                  (muse-display-warning
                   (concat
                    "Cannot find 'Page published by Emacs Muse ends here'.\n"
                    "You will probably need this text in your footer."))
                  (goto-char (point-max)))
                (setq desc (buffer-substring beg (point))))))
          (unless (string= desc "")
            (setq desc (concat "<![CDATA[" desc "]]>")))
          (delete-region (point-min) (point-max))
          (let ((entry (muse-style-element :entry-template)))
            (muse-insert-file-or-string entry)
            (goto-char (point-min))
            (while (search-forward "%date%" nil t)
              (replace-match (or date "") nil t))
            (goto-char (point-min))
            (while (search-forward "%title%" nil t)
              (replace-match "")
              (save-restriction
                (narrow-to-region (point) (point))
                (insert (or title "Untitled"))
                (remove-text-properties (match-beginning 0) (match-end 0)
                                        '(read-only nil rear-nonsticky nil))
                (let ((muse-publishing-current-style (muse-style "html")))
                  (muse-publish-escape-specials (point-min) (point-max)
                                                nil 'document))))
            (goto-char (point-min))
            (while (search-forward "%desc%" nil t)
              (replace-match desc nil t))
            (goto-char (point-min))
            (while (search-forward "%enclosure%" nil t)
              (replace-match
               (if (null enclosure)
                   ""
                 (save-match-data
                   (format
                    "<enclosure url=\"%s\" %stype=\"%s\"/>"
                    (if (string-match "//" enclosure)
                        enclosure
                      (concat (muse-style-element :base-url)
                              enclosure))
                    (let ((file
                           (expand-file-name enclosure
                                             (muse-style-element :path))))
                      (if (file-readable-p file)
                          (format "length=\"%d\" "
                                  (nth 7 (file-attributes file)))
                        ""))
                    (if (string-match "\\.\\([^.]+\\)$" enclosure)
                        (let* ((ext (match-string 1 enclosure))
                               (type
                                (assoc
                                 ext muse-journal-rss-enclosure-types-alist)))
                          (if type
                              (cdr type)
                            "application/octet-stream"))))))
               nil t))
            (goto-char (point-min))
            (while (search-forward "%link%" nil t)
              (replace-match
               (concat (muse-style-element :base-url)
                       (concat (muse-page-name)
                               muse-html-extension))
               nil t))
            (goto-char (point-min))
            (while (search-forward "%anchor%" nil t)
              (replace-match
               (muse-journal-anchorize-title (or title orig-date))
               nil t))
            (goto-char (point-min))
            (while (search-forward "%maintainer%" nil t)
              (replace-match
               (or (muse-style-element :maintainer)
                   (concat "webmaster@" (system-name)))
               nil t)))))))
  ;; indicate that we are to continue the :before-end processing
  nil)

(provide 'dim-muse-hack)
