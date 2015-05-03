;;;
;;; Rendering web pages from fragments, including headers, footers and muse
;;; articles to parse on-disk
;;;

(in-package #:tapoueh)

(defparameter *articles-per-index* 5
  "How many articles fit in a default index page")

(defparameter *default-file-name* "index")
(defparameter *default-file-type* "html")

;;
;; Render documents, with header and footer
;;
(defun render-muse-document (&key article
			       (*script-name* (or *script-name*
						  (muse-url article))))
  "Parse the muse document at PATHNAME and spits out HTML"
  (let* ((pathname            (if article (muse-pathname article)
				  (muse-source *script-name*)))
	 (*muse-current-file* (or article
				  (muse-parse-article pathname))))
    (maybe-add-article-to-list pathname)
    (concatenate 'string
		 (ssi-file *header*)
		 (to-html *muse-current-file*)
		 (ssi-file *footer*))))

(defun render-index-page (&optional (*script-name* *script-name*) article-list)
  "Produce a listing of articles for a given index location"
  (let* ((pathname            (muse-source *script-name*)))
    (concatenate 'string
		 (ssi-file *header*)
		 (article-list-to-html-with-chapeau
		  (reverse
		   (or article-list
		       (find-blog-articles (directory-namestring pathname)))))
		 (ssi-file *footer*))))

(defun render-reversed-index-page (&optional
				     (*script-name* *script-name*)
				     article-list
				     (n *articles-per-index*))
  "Produce the main blog article listing page."
  (concatenate 'string
	       (ssi-file *header*)
	       (article-list-to-html-with-chapeau
		(reverse (last (or article-list
				   (find-blog-articles *blog-directory*)) n)))
	       (ssi-file *footer*)))

(defun render-confs-index-page (&optional
				  (*script-name* *script-name*)
				  article-list)
  "Produce the main blog article listing page."
  (concatenate 'string
	       (ssi-file *header*)
	       (to-html (muse-parse-article *conferences*))
	       (article-list-to-html-with-chapeau
		(or article-list
		    (find-blog-articles *confs-directory*)))
	       (ssi-file *footer*)))

(defun render-projects-index-page (&optional (*script-name* *script-name*)
                                     pgsql-article-list
                                     emacs-article-list)
  "Produce the projects page, with two sections, PostgreSQL and Emacs."
  (concatenate 'string
               (ssi-file *header*)
	       (to-html (muse-parse-article *projects*))
               (to-html (muse-parse-article *pgsql*))
               (article-list-to-html-with-chapeau
                (or pgsql-article-list
                    (find-muse-documents :base-directory *pgsql-directory*)))
               (to-html (muse-parse-article *emacs*))
               (article-list-to-html-with-chapeau
		(or emacs-article-list
                    (find-muse-documents :base-directory *emacs-directory*)))
               (ssi-file *footer*)))

(defun render-tag-cloud ()
  "Produce our tags cloud"
  (json:encode-json-to-string (tags-cloud)))

(defun render-tag-listing (&optional (*script-name* *script-name*) article-list)
  "Produce a listing of articles for given tag"
  (let ((tag-name (second (split-pathname *script-name*))))
    (concatenate 'string
		 (ssi-file *header*)
		 (article-list-to-html-with-chapeau
		  (reverse
		   (or article-list
		       (find-blog-articles-with-tag *blog-directory* tag-name))))
		 (ssi-file *footer*))))

(defun render-rss-feed (&optional stream
                          (*script-name* *script-name*) article-list)
  "Produce the RSS feed for the given tag, or all articles"
  ;; filter out the type (.xml) for backward compatibility
  (let ((tag-name (pathname-name (second (split-pathname *script-name*)))))
    (article-list-to-rss
     stream
     (reverse
      (or article-list
	  ;; the RSS stream should contain the full article
	  (find-blog-articles-with-tag *blog-directory* tag-name
				       :parse-fn #'muse-parse-article)))
     :tag tag-name)))
