;;; tapoueh.el --- Muse Setup for tapoueh.org
;;
(require 'muse-mode)
(require 'muse-html)

(defvar tapoueh-base-dir "/Users/dim/dev/tapoueh.org/")
(defvar tapoueh-base-url "http://localhost:8042/")

(defun tapoueh-current-url ()
  "Return the URL of the current file, or NIL if not under tapoueh-base-dir"
  (let* ((path   (buffer-file-name))
	 (match  (string-match (concat "^" tapoueh-base-dir "\\(.*\\)") path))
	 (script (when match (match-string 1 path)))
	 (url    (when script (concat tapoueh-base-url script))))
    url))

;;;
;;; Facilities
;;;
(defun tapoueh-insert-muse-headers ()
  "insert headers in a new Muse file"
  (interactive)
  (when (tapoueh-current-url)
    (beginning-of-buffer)
    (unless
	(or (bound-and-true-p muse-publishing-current-file)
	    (and (buffer-file-name)
		 (file-exists-p (buffer-file-name))
		 (tapoueh-extract-directive "title" (muse-current-file))))
      (message "New Muse File: %s" (muse-current-file))
      (let ((title (read-string "Article Title: ")))
	(insert "#author Dimitri Fontaine\n"
		(format "#title  %s\n" title)
		(format "#date   %s\n" (format-time-string "%Y%m%d-%R"))
		"#tags   \n"
		"\n")))))

(add-hook 'muse-mode-hook 'tapoueh-insert-muse-headers)

(defun tapoueh-open-in-browser ()
  "Open current article in a browser"
  (interactive)
  (browse-url (tapoueh-current-url)))

(define-key muse-mode-map (kbd "C-c C-v") 'tapoueh-open-in-browser)

;;;
;;; C-c C-r to rsync the static website to the hosting server
;;;
(defvar dim:muse-rsync-options "-avz"
  "rsync options")

(defvar dim:muse-rsync-source "~/dev/tapoueh.org"
  "local path from where to rsync, with no ending /")

(defvar dim:muse-rsync-target
  "dim@tapoueh.org:/var/www/blog.tapoueh.org"
  "Remote URL to use as rsync target, with no ending /")

(defun dim:muse-project-rsync ()
  "publish tapoueh.org using rsync"
  (interactive)
  (let* ((rsync-command (format "rsync %s %s %s"
				dim:muse-rsync-options
				(concat dim:muse-rsync-source "/")
				(concat dim:muse-rsync-target "/"))))
    (with-current-buffer (get-buffer-create "*muse-rsync*")
      (erase-buffer)
      (insert (concat rsync-command "\n"))
      (message "%s" rsync-command)
      (insert (shell-command-to-string rsync-command))
      (insert "\n"))))

(define-key muse-mode-map (kbd "C-c C-r") 'dim:muse-project-rsync)
