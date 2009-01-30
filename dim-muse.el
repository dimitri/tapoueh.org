;;; dim-muse.el --- Emacs Muse setup to publish my projects

(require 'muse-mode)
(require 'muse-html) 
(require 'muse-journal)
(require 'muse-latex)

(require 'muse-project)  ; publication par projets

(muse-derive-style
 "tapoueh-html" "html"
 :header "~dim/dev/muse/css/header.html"
 :footer "~dim/dev/muse/css/footer.html"
 :style-sheet "<link rel=\"stylesheet\" type=\"text/css\"  media=\"all\" href=\"css/styles.css\" />") 

(muse-derive-style
 "tapoueh-journal-html" "journal-html"
 :header "~dim/dev/muse/css/blog-header.html"
 :footer "~dim/dev/muse/css/blog-footer.html"
 :style-sheet "<link rel=\"stylesheet\" type=\"text/css\"  media=\"all\" href=\"css/styles.css\" />"
 :date-format "%a, %e %b %Y, %k:%M")

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

(provide 'dim-muse)
