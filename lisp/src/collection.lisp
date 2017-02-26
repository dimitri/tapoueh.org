;;;; collection.lisp
;;;
;;; Maintain a list of all articles in this blog
;;;

(in-package #:tapoueh)

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
    (when (and (muse-p article)
	       (muse-article-p article)
	       (member "blog" (split-pathname pathname) :test #'string=))
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

(defun maybe-add-article-to-list (pathname)
  "Lookup pathname in *BLOG-ARTICLES* and add its article if not found."
  (unless (gethash pathname *blog-articles*)
    (add-article pathname :re-sort-list t)))

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
             (let ((*script-name* (muse-pathname-to-script-name pathname)))
               (push (funcall parse-fn pathname) articles))))
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
(defun format-article-list-as-rss (stream list
				   &key
				     (title       "tail -f /dev/dim")
				     (tag         "tapoueh")
				     (link        "http://tapoueh.org/blog")
				     (description "Dimitri Fontaine's blog")
				     (language    "en-us"))
  "Produce an RSS listing of the given list of articles"
  (format stream "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<rss version='2.0' xmlns:atom='http://www.w3.org/2005/Atom'>
 <channel>
   <title>~a</title>
   <link>~a</link>
   <description>~a</description>
   <language>~a</language>
   <generator>Emacs Muse and Tapoueh's Common Lisp</generator>
   <atom:link href='http://tapoueh.org/rss/~a.xml'
               rel='self'
              type='application/rss+xml' />~%"
          title link description language tag)
  (loop
     for article in list
     do (progn
          (muse-format-article-as-rss article stream)
          (format stream "~%")))
  (format stream "~& </channel>~%</rss>"))

(defun article-list-to-rss (stream list &key (tag "tapoueh"))
  "Produce a RSS feed from the given list of articles"
  (let ((cl-who:*html-empty-tags*
	 (cons :|atom:link| cl-who:*html-empty-tags*)))
    (format-article-list-as-rss stream list :tag tag)))
