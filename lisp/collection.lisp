;;;; collection.lisp
;;;
;;; Maintain a list of all articles in this blog
;;;

(in-package #:tapoueh)

(defparameter *root-directory*
  (asdf:system-relative-pathname :tapoueh "../"))

(defparameter *blog-directory*
  (asdf:system-relative-pathname :tapoueh "../blog/"))

(defparameter *html-directory*
  "/Users/dim/dev/temp/tapoueh.org/"
  "Where to publish the compiled static website")

(defparameter *header*
  (asdf:system-relative-pathname :tapoueh "../static/header.html"))

(defparameter *footer*
  (asdf:system-relative-pathname :tapoueh "../static/footer.html"))

(defvar *blog-articles* (make-hash-table :test 'equal :size 1024)
  "Hash-table of all blog articles, keyed by their pathname.")

(defvar *blog-articles-list* nil
  "List of blog articles PATHNAMEs, ordered by article date in
   find-all-blog-articles, not maintained by add-article. Not costly enough
   to maintain to bother, as of now.")

(defun compute-sorted-blog-articles-list ()
  "Set *blog-articles-list* as the sorted-by-timestamp list of
  *blog-articles* documents pathnames."
  (let ((articles (alexandria:hash-table-values *blog-articles*)))
    (setq *blog-articles-list*
	  (mapcar #'muse-pathname
		  (sort (remove-if (lambda (x) (null (muse-date x))) articles)
			#'sort-articles)))))

(defun add-article (pathname &key re-sort-list)
  "Add article found at PATHNAME into *blog-articles*.
   When RE-SORT-LIST is true, then we generate a whole new
   *blog-articles-list* version.

   Return the parsed article."
  (let ((article (muse-parse-directives pathname)))
    (when (and (muse-p article) (muse-article-p article))
      (setf (gethash pathname *blog-articles*) article)

      (when re-sort-list (compute-sorted-blog-articles-list))

      ;; return the article we just added, as a muse structure
      article)))

(defun find-all-blog-articles (&key purge)
  "Find all .muse articles from *blog-directory*, parse them and keep them
     in a hash-table where the pathname is the key."
  (when purge
    (setq *blog-articles* (make-hash-table :test 'equal :size 1024)))
  (fad:walk-directory *root-directory* #'add-article :test #'muse-file-type-p)
  (compute-sorted-blog-articles-list))

(defun maybe-parse-document (pathname)
  "Lookup PATHNAME *blog-articles*, if not found, fetch from disk and add it."
  (let ((article (gethash pathname *blog-articles*)))
    (if (or (null article) (file-newer-p article))
	(add-article pathname :re-sort-list t)
	article)))

;;;
;;; Find articles and return a sorted list of them
;;;
(defun blog-article-pathname-p (pathname)
  "Returns non-nil when PATHNAME could host a blog article"
  (and pathname
       (muse-file-type-p pathname)
       (not (string= (pathname-name pathname) "index"))))

(defun publish-document-p (pathname)
  "Returns non-nil when PATHNAME leads to a document we want to publish."
  (let* ((paths (split-pathname pathname)))
    (or (and (member "blog" paths :test #'string=)
	     (blog-article-pathname-p pathname))
	(and (muse-file-type-p pathname)
	     (not (intersection '("blog" "tags" "rss") paths :test #'string=))))))

(defun find-muse-documents (&key
			      (base-directory *root-directory*)
			      (parse-fn #'muse-parse-article))
  "Find all .muse documents in the tree and return a list of them."
  (let ((base-directory
	 (if (fad:pathname-absolute-p base-directory)
	     (fad:pathname-directory-pathname base-directory)
	     (expand-file-name-into base-directory *root-directory*)))
	articles)
    (flet ((push-article (pathname)
	     (push (funcall parse-fn pathname) articles)))
      ;; walk on-disk directories to find articles
      (fad:walk-directory base-directory #'push-article
			  :test #'publish-document-p)
      articles)))

(defun find-blog-articles (base-directory
			   &key (parse-fn #'muse-parse-chapeau))
  "Find all muse articles in given BASE-DIRECTORY, return a sorted list of
   them. Don't even have a look at files when TEST returns true, if provided."
  (let (articles)
    (flet ((push-article (pathname)
	     (let ((doc (funcall parse-fn pathname)))
	       (when (muse-article-p doc)
		 (push doc articles)))))
      ;; walk on-disk directories to find articles
      (fad:walk-directory (if (fad:pathname-absolute-p base-directory)
			      (fad:pathname-directory-pathname base-directory)
			      (expand-file-name-into base-directory
						     *root-directory*))
			  #'push-article
			  :test #'blog-article-pathname-p)
      ;; sort the articles now
      (sort articles #'muse-article-before-p))))

(defun find-blog-articles-with-tag (base-directory query-tags
				    &key (parse-fn #'muse-parse-chapeau))
  "Find all muse articles in given BASE-DIRECTORY having a #tags directive
   that contains one of the given TAGS"
  (let ((query-tags (if (stringp query-tags) (list query-tags) query-tags))
	articles)
    (flet ((push-article (pathname)
	     (let* ((doc  (funcall parse-fn pathname))
		    (tags (muse-tags doc)))
	       (when (and (muse-article-p doc)
			  ;; "tapoueh" is the catch-all tag
			  (or (member "tapoueh" query-tags :test #'string-equal)
			      (intersection query-tags tags :test #'string-equal)))
		 (push doc articles)))))
      ;; walk on-disk directories to find articles
      (fad:walk-directory (if (fad:pathname-absolute-p base-directory)
			      (fad:pathname-directory-pathname base-directory)
			      (expand-file-name-into base-directory
						     *root-directory*))
			  #'push-article
			  :test #'blog-article-pathname-p))
    ;; sort the articles now
    (sort articles #'muse-article-before-p)))

;;;
;;; Format list of articles
;;;
(defun format-article-list (list)
  "Given a LIST of muse articles (proper muse structure), return the cl-who
   forms needed to render the list to html"
  `(:ul :class "thumbnails"
    ,@(loop
	 for article in list
	 collect (muse-format-article article))))

(defun article-list-to-html (list)
  "Produce the HTML Listing of the given list of articles."
  (concatenate 'string
	       (eval `(with-html-output-to-string (s)
			,(format-article-list list)))))

;;;
;;; Article list with chapeau
;;;
(defun format-article-list-with-chapeau (list)
  "Produce the detailed HTML Listing of the given list of articles."
  `(:div
    ,@(loop
	 for article in list
	 collect (muse-format-article-with-chapeau article))))

(defun article-list-to-html-with-chapeau (list)
  "Produce the detailed HTML Listing of the given list of articles."
 (concatenate 'string
	      (eval `(with-html-output-to-string (s)
		       ,(format-article-list-with-chapeau list)))))

;;; RSS
(defun format-article-list-as-rss (list
				   &key
				     (title       "tail -f /dev/dim")
				     (link        "http://tapoueh.org/blog")
				     (description "Dimitri Fontaine's blog")
				     (language    "en-us"))
  "Produce an RSS listing of the given list of articles"
  (let ((cl-who:*prologue* "<?xml version=\"1.0\" encoding=\"utf-8\"?>"))
    `(:rss
      :version "2.0"
      (:channel
       (:title ,title)
       (:link ,link)
       (:description ,description)
       (:language ,language)
       (:generator "Emacs Muse and Tapoueh's Common Lisp")
       ,@(loop
	    for article in list
	    collect (muse-format-article-as-rss article))))))

(defun article-list-to-rss (list)
  "Produce a RSS feed from the given list of articles"
  (concatenate 'string
	       (eval `(with-html-output-to-string (s)
			,(format-article-list-as-rss list)))))

;;;
;;; Compile into a static website
;;;
(defun blog-index-script-name (pathname &optional (root *blog-directory*))
  "Return the /blog/path/to/index url from given PATHNAME"
  (concatenate 'string
	       "/blog"
	       (subseq (namestring pathname) (- (length (namestring root)) 1))))

(defun articles-tagged (tag documents)
  "Return the list of documents that have the given tag."
  (flet ((document-tagged-p (document)
	   (member tag (muse-tags document) :test #'string-equal)))

   (remove-if-not #'document-tagged-p documents)))

(defun write-html-file (html localname &key verbose name (type "html"))
  "Write a HTML file into *HTML-DIRECTORY* as localname."
  (let ((dest (expand-file-name-into localname *html-directory*
				     :name name
				     :type type)))
    (when verbose (format t "~a~%" dest))
    (ensure-directories-exist (directory-namestring dest))
    (with-open-file (s dest
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create
		       :external-format :utf-8)
      (write-string html s))))

(defun compile-home-page (&key documents verbose)
  "Compile to *HTML-DIRECTORY* this site's home page"
  (let ((url "/")
	(html (render-reversed-index-page "/" documents)))
    (write-html-file html url :name "index" :verbose verbose)))

(defun compile-tags-cloud (&key documents verbose)
  "Compile the cloud tags JSON file."
  (let ((*blog-articles* (make-hash-table :test 'equal :size 1024)))
    ;; fill-in the hash table
    (loop
       for document in documents
       do (setf (gethash (muse-pathname document) *blog-articles*) document))

    (let ((json (render-tag-cloud)))
      (write-html-file json "/" :name "cloud" :type "json" :verbose verbose))
    ;; return how many documents where processed
    (hash-table-count *blog-articles*)))

(defun compile-site-documents (&key documents verbose)
  "Compile to *HTML-DIRECTORY* all the muse documents from *ROOT-DIRECTORY*."
  (loop
     for article in (or documents (find-muse-documents))
     do (let ((html (render-muse-document :article article)))
	  (write-html-file html (muse-url article) :verbose verbose))
     ;; return how many documents we wrote
     count article))

(defun compile-blog-indexes (&key documents verbose)
  "Compile to *HTML-DIRECTORY* all the blog indexes from *BLOG-DIRECTORY*"
  (let ((count 0))
    (flet ((write-index-file (pathname)
	     (let* ((url  (blog-index-script-name pathname))
		    (html (if (string= "/blog/" url)
			      (render-reversed-index-page url documents 7)
			      (render-index-page url))))
	       (incf count)
	       (write-html-file html url :name "index" :verbose verbose))))

      (fad:walk-directory *blog-directory* #'write-index-file
			  :directories t :test #'fad:directory-pathname-p))
    ;; return how many indexes we just built
    count))


(defun compile-tags-lists (&key documents verbose)
  "Compile to *HTML-DIRECTORY* all the per-tag articles listings."
  (let ((tags))
    ;; prepare the list of known tags
    (loop
       for article in documents
       do (loop for tag in (muse-tags article)
	       do (pushnew tag tags :test #'string-equal)))
    ;; now export tags listings
    (loop
       for tag in (mapcar #'string-downcase tags)
       do (let* ((url  (format nil "/tags/~a" tag))
		 (html (render-tag-listing url (articles-tagged tag documents))))
	    (write-html-file html url :name tag :verbose verbose))
       ;; return how many files we wrote
       count tag)))

(defun compile-rss-feeds (&key documents verbose
			    (tags '("tapoueh"
				    "postgresql"
				    "postgresqlfr"
				    "emacs"
				    "debian")))
  "Compile to *HTML-DIRECTORY* all the rss feeds"
  (loop
     for tag in (mapcar #'string-downcase tags)
     do (let* ((url  (format nil "/rss/~a" tag))
	       (rss (render-rss-feed url (articles-tagged tag documents))))
	  (write-html-file rss url :name tag :type "xml" :verbose verbose))
     ;; return how many files we wrote
     count tag))

(defun compile-articles (&key verbose)
  "Output all the articles found in *ROOT-DIRECTORY* into *HTML-DIRECTORY*."
  (let* ((all-documents
	  (displaying-time ("parsed ~d docs in ~ds~%" (length result) timing)
	    (find-muse-documents)))
	 (blog-articles
	  (displaying-time ("parsed chapeau of ~d blog articles in ~ds~%"
			    (length result) timing)
	    (find-blog-articles *blog-directory*))))

    (displaying-time ("compiled the home page in ~ds~%" timing)
      (compile-home-page :documents blog-articles :verbose verbose))

    (displaying-time ("compiled the tags cloud in ~ds~%" timing)
      (compile-tags-cloud :documents blog-articles :verbose verbose))

    (displaying-time ("compiled ~d documents in ~d secs~%" result timing)
      (compile-site-documents :documents all-documents :verbose verbose))

    (displaying-time ("compiled ~d blog indexes in ~ds~%" result timing)
      (compile-blog-indexes :documents blog-articles :verbose verbose))

    (displaying-time ("compiled ~d tag listings in ~ds~%" result timing)
      (compile-tags-lists :documents blog-articles :verbose verbose))

    (displaying-time ("compiled ~d rss feeds in ~ds~%" result timing)
      (compile-rss-feeds :documents all-documents :verbose verbose))))

