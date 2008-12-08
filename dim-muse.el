;;; dim-muse.el --- Emacs Muse setup to publish my projects

(require 'muse-mode)
(require 'muse-html) 
(require 'muse-journal)

(require 'muse-project)  ; publication par projets

(muse-derive-style
 "tapoueh-html" "html"
 :header "../css/header.html"
 :footer "../css/footer.html"
 :style-sheet "<link rel=\"stylesheet\" type=\"text/css\"  media=\"all\" href=\"../../css/styles.css\" />") 

(muse-derive-style
 "tapoueh-journal-html" "journal-html"
 :header "../css/blog-header.html"
 :footer "../css/blog-footer.html"
 :style-sheet "<link rel=\"stylesheet\" type=\"text/css\"  media=\"all\" href=\"../css/styles.css\" />") 

(setq muse-project-alist
      '(("pgsql.tapoueh.org" ("~/dev/muse/site" :default "index")
	 (:base "tapoueh-html" 
		:path "~/dev/muse/out/site"))
	
	("blog.tapoueh.org" ("~/dev/muse/blog")
	 (:base "tapoueh-journal-html" 
		:path "~/dev/muse/out/blog")
	 (:base "journal-rss" 
		:base-url "http://blog.tapoueh.org/" 
		:path "~/dev/muse/out/blog"))))

(provide 'dim-muse)
