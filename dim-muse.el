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
 :header "~dim/dev/tapoueh.org/css/header.html"
 :footer "~dim/dev/tapoueh.org/css/footer.html"
 :style-sheet "<link rel=\"stylesheet\" type=\"text/css\"  media=\"all\" href=\"css/styles.css\" />") 

(muse-derive-style
 "tapoueh-journal-html" "journal-html"
 :header "~dim/dev/tapoueh.org/css/header.html"
 :footer "~dim/dev/tapoueh.org/css/footer.html"
 :style-sheet "<link rel=\"stylesheet\" type=\"text/css\"  media=\"all\" href=\"css/styles.css\" />"
 :date-format "%a, %e %b %Y, %k:%M"
 :date-format-notime "%a, %e %b %Y"
 :final 'tapoueh-journal-html-split-entries)

(muse-derive-style
 "tapoueh-pdf" "pdf"
 :header "~dim/dev/tapoueh.org/css/latex-header.tex")

(setq muse-project-alist
      '(("pgsql.tapoueh.org" ("~/dev/tapoueh.org/site" :default "index")
	 (:base "tapoueh-html" 
		:path "~/dev/tapoueh.org/out/")

	 (:base "pdf"
	 	:path "~/dev/tapoueh.org/pdf/"))
	
	("blog.tapoueh.org" ("~/dev/tapoueh.org/blog")
	 (:base "tapoueh-journal-html" 
		:path "~/dev/tapoueh.org/out/")

	 (:base "journal-rss" 
		:base-url "http://blog.tapoueh.org/" 
		:path "~/dev/tapoueh.org/out/"))))

(defun dim:muse-get-header-and-start ()
  "return the header string and its end position"
  (goto-char (point-min))
  (re-search-forward "<div class=\"entry\">")
  (beginning-of-line)
  (cons (buffer-substring-no-properties 1 (point)) (point)))

(defun dim:muse-get-footer-and-end ()
  "return the footer string and its beginning position"
  (goto-char (point-max))
  (re-search-backward "<div id=\"footer\">")
  (forward-line -2)
  (cons (buffer-substring-no-properties (point) (point-max)) (point)))

(defun dim:muse-collect-entries (start end)
  "return a list of region positions where to find entries"
  (goto-char start)
  (let ((positions '())
	(name              "")
	(date              "")
	(current-position  (point))
	(previous-position (point)))
    (while (re-search-forward "<div class=\"entry\">" end t 2)
      (forward-line -1)
      (setq current-position (point))

      ;; get the name too, and have a filename version of it
      (goto-char previous-position)
      (let* ((hs (re-search-forward "<h2>"))
	     (he (re-search-forward "</h2>"))
	     (an (buffer-substring-no-properties hs (- he 5))))
	(setq name
	      (cons an (mapconcat 'identity 
				  (split-string an "[ /\\]\\|&amp;") "_"))))

      (setq positions 
	    (cons (list name previous-position current-position) positions))
      (setq previous-position current-position)
      (goto-char current-position))
    (reverse positions)))

(defun dim:muse-make-index (header footer articles)
  "return the index content (string) from the article list, header and footer"
  (concat header
	  "<ul>\n"
	  (mapconcat 
	   (lambda (article)
	     (let ((name  (caar  article))
		   (fname (cdar  article))
		   (start (cadr  article))
		   (end   (caddr article)))
	       ;; fetch the date, good to have in the index
	       (goto-char start)
	       (let* ((ds (re-search-forward "<span class=\"date\">"))
		      (de (re-search-forward "</span>"))
		      (ad (buffer-substring-no-properties ds (- de 7))))
		 (format 
		  "<li><a href=\"%s.html\">%s</a>, %s</li>\n" fname name ad))))
	     articles "\n")
	  "</ul>\n"
	  footer))

(defun tapoueh-journal-html-split-entries (source target sstarget)
  "Split HTML output in one file per entry, called as a Muse :final function"
  (save-excursion
    (let* ((header-and-start (dim:muse-get-header-and-start))
	   (header           (car header-and-start))
	   (start            (cdr header-and-start))
	   (footer-and-end   (dim:muse-get-footer-and-end))
	   (footer           (car footer-and-end))
	   (end              (cdr footer-and-end))
	   (articles         (dim:muse-collect-entries start end))
	   (index            (dim:muse-make-index header footer articles))
	   (dir 
	    (file-name-as-directory
	     (concat 
	      (file-name-directory target) 
	      "articles/"
	      (car (split-string (file-name-nondirectory target) "[.]"))))))
      ;; write the index
      (write-region index nil (concat dir "index.html"))
      ;; produce one file per article
      (dolist (a articles)
	(let ((name    (concat dir (cdar a) ".html"))
	      (from    (cadr a))
	      (to      (caddr a)))
	  (write-region header nil name)
	  (write-region from to name 'append)
	  (write-region footer nil name 'append)
	  (message "tapoueh-journal-html-split-entries: %s." name))))))

(defvar dim:muse-rsync-options "-avz"
  "rsync options")

(defvar dim:muse-rsync-source "~/dev/tapoueh.org/out"
  "local path from where to rsync, with no ending /")

(defvar dim:muse-rsync-target
  "dim@tapoueh.org:/home/www/tapoueh.org/blog.tapoueh.org"
  "Remote URL to use as rsync target, with no ending /")

(defvar dim:muse-rsync-extra-subdirs
  '("../css" "../images" "../pdf" "../static")
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
(define-key muse-mode-map (kbd "C-c C-r") 'dim:muse-project-rsync)

(provide 'dim-muse)
