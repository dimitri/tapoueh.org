;;;; muse.lisp
;;;
;;; Parse the Emacs Muse format from the Muse project, as found at:
;;;
;;;   http://mwolson.org/projects/EmacsMuse.html

(in-package #:tapoueh)

(defparameter *base-url* "http://tapoueh.org/"
  "Used in the headers Meta tags and in the RSS feeds.")

(defparameter *muse-pathname-type* "muse"
  "The file extension we use to store Muse files")

(defparameter *image-directory*
  (asdf:system-relative-pathname :tapoueh "../images/"))

(defparameter *image-type-list*
  (remove-if
   #'null
   (remove-duplicates (mapcar #'pathname-type
			      (fad:list-directory *image-directory*))
		      :test #'string=)))

(defparameter *article-default-image* "/images/article2.gif"
  "Full URL of the default image to use in article listings.")

(defparameter *article-default-image-for-tag*
  '(("Conferences"  . "/images/conferences.jpg")
    ("pgcon"        . "/images/conferences.jpg")
    ("debian"       . "/images/debian-logo.png")
    ("catalogs"     . "/images/library-card-catalogs.small.jpg")
    ("tricks"       . "/images/tips-and-tricks.jpg")
    ("Skytools"     . "/images/londiste_logo.gif")
    ("PostgreSQLFr" . "/images/postgresqlfr-logo.png")
    ("pgloader"     . "/images/toy-loader.320.jpg")
    ("PostgreSQL"   . "/images/postgresql-elephant.small.png")
    ("Common-Lisp"  . "/images/made-with-lisp.png")
    ("El-Get"       . "/images/el-get.big.png")
    ("Emacs"        . "/images/emacs-logo.png"))
  "An alist of default image for article listings, by tag")

(defvar *muse-parser-cwd* nil
  "Muse Current Working Directory, for <include> blocks")

;;
;; Document data structure, pretty loose
;;
(defstruct
    (muse
      (:constructor make-muse)
      (:constructor make-muse-article (&key
				       pathname mod-timestamp
				       author title date tags desc
				       contents first-para image
				       &aux (timestamp (when date
							 (parse-date date))))))
  pathname mod-timestamp author title date timestamp tags desc
  contents first-para image)

(defmethod muse-article-p ((document muse))
  "Return a generalized boolean true when DOCUMENT is a Muse Article.

   A Muse Article is a Muse Document with all the directives given, so that
   none of the meta-data slots are nil."
  (and (muse-author document)
       (muse-title  document)
       (muse-date   document)
       (muse-tags   document)))

;;; a .muse file is a "document"
(defmethod same-document-p ((d1 muse) (d2 muse))
  "Compare d1 and d2 slots"
  (and (equalp (muse-author d1) (muse-author d2))
       (equalp (muse-title d1) (muse-title d2))
       (equalp (muse-date d1) (muse-date d2))
       (local-time:timestamp= (muse-timestamp d1) (muse-timestamp d2))
       (tree-equal (muse-tags d1) (muse-tags d2) :test #'equal)
       (tree-equal (muse-contents d1) (muse-contents d2) :test #'equal)))

(defmethod file-newer-p ((m muse))
  "True when the modification time of p1 is before the p2 one"
  (local-time:timestamp< (muse-mod-timestamp m)
			 (local-time:universal-to-timestamp
			  (file-write-date (muse-pathname m)))))

(defmethod to-html ((document muse))
  "Produce the HTML of given Document"
  (eval `(with-html-output-to-string (s)
	   ,@(muse-contents document))))

(defmethod muse-article-before-p ((a1 muse) (a2 muse))
  "Return a generalized boolean true when a1 has a date in the past of a2's"
  (local-time:timestamp< (muse-timestamp a1)
			 (muse-timestamp a2)))

;;; a .muse file with the 4 directives is an "article"
(defmethod sort-articles ((a1 muse) (a2 muse)
			  &key
			    (test #'local-time:timestamp<)
			    (key 'timestamp))
  (let ((t1 (slot-value a1 key))
	(t2 (slot-value a2 key)))
    (when (and t1 t2)
      (funcall test t1 t2))))

(defun muse-parse-article (pathname)
  "Parse the Muse article at PATHNAME and return a muse structure."
  (let* ((*muse-parser-cwd* (directory-namestring pathname))
	 (article (parse 'article (slurp-file-into-string pathname))))
    (setf (muse-pathname article) pathname)
    (setf (muse-mod-timestamp article)
	  (local-time:universal-to-timestamp (file-write-date pathname)))
    article))

(defun muse-parse-directives (pathname)
  "Only parse the Muse directives, not the whole document"
  (let ((document
	 (parse 'directives (slurp-file-into-string pathname) :junk-allowed t)))
    (if document
	(progn
	  (setf (muse-pathname document) pathname)
	  (setf (muse-mod-timestamp document)
		(local-time:universal-to-timestamp (file-write-date pathname)))
	  document)
	(make-muse :pathname pathname))))

(defun muse-parse-chapeau (pathname)
  "Only parse the Muse directives, not the whole document"
  (let ((document
	 (parse 'chapeau (slurp-file-into-string pathname) :junk-allowed t)))
    (if document
	(progn
	  (setf (muse-pathname document) pathname)
	  (setf (muse-mod-timestamp document)
		(local-time:universal-to-timestamp (file-write-date pathname)))
	  document)
	(make-muse :pathname pathname))))

(defun muse-file-type-p (pathname)
  "Returns a generalized boolean true when pathname extension is .muse"
  (string= (pathname-type pathname) *muse-pathname-type*))

(defmethod muse-format-date ((m muse) &key (format :normal))
  "Format muse-date to be displayed on the web"
  (let ((stamp (muse-timestamp m)))
    (when stamp
      (local-time:format-timestring
       nil stamp
       :format
       (case format
	 (:normal '(:long-weekday ", " :long-month " " (:day 2) " " :year))
	 (:long   '(:long-weekday ", " :long-month " " (:day 2) " " :year
		    ", " (:hour 2) ":" (:min 2)))
	 (:short  '(:long-month ", " (:day 2) " " :year))
	 (:rss    local-time:+rfc-1123-format+)
	 (:sitemap '(:year (:day 2) (:month 2) "-" (:hour 2) ":" (:min 2))))))))

(defmethod muse-format-tags ((m muse))
  "Format muse-tags to be displayed on the web"
  (loop
     for (tag . more?) on (muse-tags m)
     collect `(:a :href (format nil "/tags/~a" ,(string-downcase tag)) ,tag)
     when more? collect ", "))

(defmethod muse-extract-article-image-source ((article muse))
  "Extract the image source from the article"
  (let* ((tags  (muse-tags article))
	 (image (muse-image article)))
    (labels ((image-link-p (image)
	       (and (listp image) (eq :img (car image))))
	     (image-link-or-nil (image)
	       (when (image-link-p image) image)))
      ;; if the image is itself a link, discard the link
      ;; (:A :HREF "https://fosdem.org/2013/"
      ;;     (:IMG :SRC "../../../images/fosdem.png"))
      (or (cond ((and (listp image)
		      (eq :a (car image)))    (image-link-or-nil (fourth image)))
		((image-link-p image)         image))

	  ;; grab the main article tag image if we have one
	  (loop
	     for (tag . image-file) in *article-default-image-for-tag*
	     when (member tag tags :test #'string-equal)
	     return `(:img :src ,image-file))

	  ;; default to the global default
	  `(:img :src ,*article-default-image*)))))

(defmethod muse-format-article ((article muse))
  "Return a list suitable for printing the article meta-data with cl-who"
  (let* ((link `(:a :href ,(muse-url article)
		    ,(muse-title article)))
	 (date `(:span :class "date" ,(muse-format-date article :format :short)))
	 (image (muse-extract-article-image-source article)))
    `(:li :class "span2"
	  (:div :class "thumbnail"
		(:a :class "thumbnail" :href ,(muse-url article)
		    (:img :style "width: 160px; height: 120px;"
			  :src ,(if (listp image) (third image) image)))
		(:h4 ,link)
		(:div :class "date" ,date)))))

(defmethod muse-format-article-with-chapeau ((article muse))
  "Return a list suitable for printing the article meta-data with cl-who"
  (let* ((link `(:a :href ,(muse-url article)
		    ,(muse-title article)))
	 (date `(:span :class "date" ,(muse-format-date article :format :short)))
	 (image (muse-extract-article-image-source article)))
    `(:div :class "row"
	   (:div :class "span2" (:p "&nbsp;"))

	   (:div :class "span6"
		 (:h2 ,link)
		 (:div :class "date" (:i :class "icon-calendar") " " ,date))

	   (:div :class "span2"
		 (:a :class "thumbnail" :href ,(muse-url article)
		     (:img :class "img-polaroid"
			   :style "width: 160px; height: 120px;"
			   :src ,(if (listp image) (third image) image))))

	   (:div :class "span6" ,(muse-first-para article)))))

(defun relative-href-to-absolute (script-name href)
  "Transform relative HREF found at SCRIPT-NAME into an absolute reference."
  (if (not (string= ".." (subseq href 0 2)))
      href
      (let* ((cwd         (butlast (split-pathname script-name)))
	     (relpaths    (split-pathname href)))
	(loop
	   for dest = (reverse cwd) then (cdr dest)
	   for (p . rest) on relpaths
	   until (not (string= p ".."))
	   finally (return
		     (format nil "~a~{~a~^/~}"
			     *base-url*
			     (append (reverse dest) (list p) rest)))))))

(defmethod muse-contents-with-absolute-hrefs ((article muse))
  "Return the muse contents after having replaced all relative hrefs with
   absolute ones, using *BASE-URL*"
  (labels ((convert-html-tag (a-form tag)
	     "Convert an HTML tag's URL, tag is e.g. :href or :src"
	     (loop
		for convert-p = nil then (eq attr tag)
		for attr in a-form
		when convert-p
		collect (relative-href-to-absolute (muse-url article) attr)
		;; beware of (:a :href "../..." (:img :src "../..."))
		else collect (if (listp attr) (walk-contents (list attr)) attr)))

	   (walk-contents (contents)
	     "Walk the muse contents"
	     (loop
		for element in contents
		collect (cond
			  ((and (listp element) (eql (car element) :a))
			   (convert-html-tag element :href))

			  ((and (listp element) (eql (car element) :img))
			   (convert-html-tag element :src))

			  ((listp element)
			   (walk-contents element))

			  (t element)))))
    (walk-contents (muse-contents article))))

(defmethod muse-format-article-as-rss ((article muse))
  "Return a list suitable for printing the article as a RSS form with cl-who"
  (let* ((title  (muse-title article))
	 (url    (muse-url article :with-base-url t :with-file-type "html"))
	 (date   (muse-format-date article :format :rss))
	 (author "dim@tapoueh.org (Dimitri Fontaine)"))
    ;; (desc   (concatenate 'string "<![CDATA[" (to-html article) "]]>"))
    `(:item
      (:title ,(who:escape-string title))
      (:link ,url)
      (:description (str "<![CDATA[")
		    ,@(muse-contents-with-absolute-hrefs article)
		    (str "]]>"))
      (:author ,author)
      ;; easiest way to respect the case here
      (str "<pubDate>") ,date (str "</pubDate>")
      (str "<guid isPermaLink=\"true\">") ,date (str "</guid>"))))
