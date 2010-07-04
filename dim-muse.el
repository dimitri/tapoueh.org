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
 :after 'tapoueh-journal-html-index
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

;;;
;;; tapoueh-journal-html-split-entries will produce a HTML page per article
;;; in the blog, and an index page to reference them, in a
;;; subdirectory. Another index is also generated to be put on the main
;;; page, where we keep only dim:muse-most-recent-articles then put the index.
;;;
(defcustom dim:muse-index-length 3
  "Keep that many articles from the blog on the main page"
  :type 'int)

(defun dim:muse-get-header-and-start ()
  "return the header string and its end position"
  (goto-char (point-min))
  (unless (re-search-forward "<div class=\"toc\">" nil t)
    (re-search-forward "<div class=\"entry\">"))
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

  ;; skip our "Previous Articles" section
  (when (re-search-forward "<div class=\"toc\">" nil t)
    (re-search-forward "<div class=\"entry\">")
    (forward-line -1))

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

(defun dim:muse-relocate-links (header &optional depth)
  "edit the HTML relative links in the header string to depth up-levels"
  (if (and depth (> depth 1))
      (progn
	(let ((up        "../")
	      (relocated "")
	      (pos       0))
	  (dotimes (d (- depth 1)) (setq up (concat up "../")))

	  (while (string-match "href=\"\\|src=\"" header pos)
	    (setq 
	     relocated (concat relocated (substring header pos (match-end 0))))
	    (setq pos (match-end 0))
	    ;; get the link
	    (string-match "\"" header pos)
	    (let ((link (substring header pos (match-end 0))))
	      (if (not (string-match-p "https?://\\|mailto:" link))
		  (setq relocated (concat relocated up link))
		(setq relocated (concat relocated link))))
	    ;; continue advancing
	    (setq pos (match-end 0)))

	  ;; return the edited header
	  (concat relocated (substring header pos))))
    header))

(defun dim:muse-make-index (articles &optional subdir n)
  "return the index content (string) from the article list, header and footer"
  (concat "<ul>\n"
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
		  "<li><a href=\"%s.html\">%s</a>, %s</li>\n" 
		  (if subdir
		      (concat (file-name-as-directory subdir) fname)
		    fname)
		  name ad))))
	   (if n 
	       (cdr (butlast articles (- (length articles) n 1)))
	     articles) 
	   "\n")
	  "</ul>\n"))

(defun tapoueh-journal-html-index ()
  ":after function for Muse, to add in the output buffer an articles index"
  (let* ((subdir 
	  (concat 
	   "articles/" 
	   (car (split-string
		 (file-name-nondirectory 
		  muse-publishing-current-output-path) "[.]"))))
	 (header-and-start (dim:muse-get-header-and-start))
	 (header           (car header-and-start))
	 (start            (cdr header-and-start))
	 (footer-and-end   (dim:muse-get-footer-and-end))
	 (footer           (car footer-and-end))
	 (end              (cdr footer-and-end))
	 (articles         (dim:muse-collect-entries start end))
	 (index            (dim:muse-make-index 
			    articles subdir dim:muse-index-length)))
    (goto-char start)
    (insert 
     (concat
      "<div class=\"toc\"><h2>Previous Articles</h2>\n"
      index
      "\n"
      (format 
       "There's also an <a href=\"%s/index.html\">index of all entries</a>."
       subdir)
      "</div>\n\n"))))

(defun tapoueh-journal-html-split-entries (source target sstarget)
  "Split HTML output in one file per entry, called as a Muse :final function"
  (save-excursion
    (let* ((header-and-start (dim:muse-get-header-and-start))
	   (header-orig      (car header-and-start))
	   (header           (dim:muse-relocate-links header-orig 2))
	   (start            (cdr header-and-start))
	   (footer-and-end   (dim:muse-get-footer-and-end))
	   (footer           (car footer-and-end))
	   (end              (cdr footer-and-end))
	   (articles         (dim:muse-collect-entries start end))
	   (index            (dim:muse-make-index articles))
	   (subdir
	    (concat "articles/" 
		    (car (split-string (file-name-nondirectory target) "[.]"))))
	   (dir 
	    (file-name-as-directory
	     (concat (file-name-directory target) subdir))))

      ;; write the index
      (write-region (concat header index footer) nil (concat dir "index.html"))

      ;; produce one file per article
      (dolist (a articles)
	(let ((name    (concat dir (cdar a) ".html"))
	      (from    (cadr a))
	      (to      (caddr a)))
	  (write-region header nil name)
	  (write-region 
	   (dim:muse-relocate-links
	    (buffer-substring-no-properties from to) 2) nil name 'append)

	  ;; add a local index
	  (write-region 
	   (concat "<div class=\"entry\"><h2>Index of All Articles</h2>\n"
		   index
		   "</div>\n\n") nil name 'append)

	  (write-region footer nil name 'append)
	  (message "tapoueh-journal-html-split-entries: %s." name))))))

;;;
;;; C-c C-r to rsync the static website to the hosting server
;;;
(defvar dim:muse-rsync-options "-avz"
  "rsync options")

(defvar dim:muse-rsync-source "~/dev/tapoueh.org/out"
  "local path from where to rsync, with no ending /")

(defvar dim:muse-rsync-target
  "dim@tapoueh.org:/var/www/blog.tapoueh.org"
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
