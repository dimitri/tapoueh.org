;;; dim-muse.el --- Emacs Muse setup to publish my projects

(require 'muse-mode)
(require 'muse-html) 
(require 'muse-journal)

(require 'muse-project)  ; publication par projets

(setq muse-project-alist
      '(("pgsql.tapoueh.org" ("~/dev/muse/site" :default "index")
	 (:base "html" 
		;; FIXME: try to properly link to the stylesheet
		:muse-html-style-sheet "../styles.css"
		:path "~/dev/muse/out/site"
		))
	
	("blog.tapoueh.org" ("~/dev/muse/blog")
	 (:base "journal-html" 
		:path "~/dev/muse/out/blog")
	 (:base "journal-rss" 
		:base-url "http://blog/tapoueh.org/" 
		:path "~/dev/muse/out/blog"))))

(provide 'dim-muse)
