;;; dim-muse.el --- Emacs Muse setup to publish my projects

(require 'muse-mode)
(require 'muse-html) 
(require 'muse-journal)
(require 'muse-latex)
(require 'muse-context)

(require 'muse-project)  ; publication par projets

(require 'dim-muse-hack)

;; toolset paths
(when (string-match "apple-darwin" system-configuration)
  (setenv "PATH" (concat (getenv "PATH") ":/sw/bin"))
  (setenv "TEXINPUTS" 
	  (concat (getenv "TEXINPUTS") 
		  "."
		  ":/usr/local/texlive/2007/texmf-dist//" 
		  ":/sw/share/texmf-dist//")))

(muse-derive-style
 "tapoueh-html" "html"
 :header "~dim/dev/muse/css/header.html"
 :footer "~dim/dev/muse/css/footer.html"
 :style-sheet "<link rel=\"stylesheet\" type=\"text/css\"  media=\"all\" href=\"css/styles.css\" />") 

(muse-derive-style
 "tapoueh-journal-html" "journal-html"
 :header "~dim/dev/muse/css/header.html"
 :footer "~dim/dev/muse/css/footer.html"
 :style-sheet "<link rel=\"stylesheet\" type=\"text/css\"  media=\"all\" href=\"css/styles.css\" />"
 :date-format "%a, %e %b %Y, %k:%M"
 :date-format-notime "%a, %e %b %Y")

(muse-derive-style
 "tapoueh-pdf" "pdf"
 :header "~dim/dev/muse/css/latex-header.tex")

(setq muse-project-alist
      '(("pgsql.tapoueh.org" ("~/dev/muse/site" :default "index")
	 (:base "tapoueh-html" 
		:path "~/dev/muse/out/")

	 (:base "pdf"
	 	:path "~/dev/muse/pdf/"))
	
	("blog.tapoueh.org" ("~/dev/muse/blog")
	 (:base "tapoueh-journal-html" 
		:path "~/dev/muse/out/")

	 (:base "journal-rss" 
		:base-url "http://blog.tapoueh.org/" 
		:path "~/dev/muse/out/"))))

(defvar dim:muse-rsync-options "-avz"
  "rsync options")

(defvar dim:muse-rsync-source "~/dev/muse/out"
  "local path from where to rsync, with no ending /")

(defvar dim:muse-rsync-target
  "dim@tapoueh.org:/home/www/tapoueh.org/blog.tapoueh.org"
  "Remote URL to use as rsync target, with no ending /")

(defvar dim:muse-rsync-extra-subdirs
  '("../css" "../images" "../pdf")
  "static subdirs to rsync too, path from dim:muse-rsync-source, no ending /")

(defun dim:muse-project-rsync (&optional static)
  "publish tapoueh.org using rsync"
  (interactive "P")
  (let* ((rsync-command (format "rsync %s %s %s" 
				dim:muse-rsync-options
				(concat dim:muse-rsync-source "/")
				(concat dim:muse-rsync-target "/"))))
    (with-current-buffer (get-buffer-create "*muse-rsync*")
      (erase-buffer)
      (insert (concat rsync-command "\n"))
      (message "%s" rsync-command)
      (insert (shell-command-to-string rsync-command))
      (insert "\n")

      (when static
	(dolist (subdir dim:muse-rsync-extra-subdirs)
	  (let ((cmd (format "rsync %s %s %s" 
			     dim:muse-rsync-options
			     (concat dim:muse-rsync-source "/" subdir)
			     dim:muse-rsync-target)))
	    (insert (concat cmd "\n"))
	    (message "%s" cmd)
	    (insert (shell-command-to-string cmd))
	    (insert "\n")))))))

(define-key muse-mode-map (kbd "C-c R") 'dim:muse-project-rsync)

(provide 'dim-muse)
