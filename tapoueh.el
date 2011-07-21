;;; tapoueh.el --- Muse Setup for tapoueh.org
;;
(require 'muse-mode)
(require 'muse-project)  ; publication par projets

(setq muse-project-alist nil)

(add-to-list
 'muse-project-alist
 `("tapoueh.org"
   (,@(loop for d in (muse-project-alist-dirs "~/dev/tapoueh.org/")
	    unless (string-match (rx (or "images" "rss")) d)
	    collect d)
    :default "index"
    :force-publish ("index"))

   ,@(muse-project-alist-styles
      "~/dev/tapoueh.org/"
      "~/dev/tapoueh.org/"
      "tapoueh-html"
      :default "index.muse"
      :base-url "http://tapoueh.org/")))

(muse-derive-style
 "tapoueh-html" "html"
 :before #'tapoueh-add-tag-links
 :after #'tapoueh-set-coding-system
 :final #'tapoueh-add-item-to-rss
 :header "~/dev/tapoueh.org/static/header.html"
 :footer "~/dev/tapoueh.org/static/footer.html"
 :suffix ".html"
 :style-sheet "<link rel=\"stylesheet\" type=\"text/css\"  media=\"all\" href=\"static/styles.css\" />")

(defvar tapoueh-rss-tags '(("rss/postgresql.xml"   . postgresql)
			   ("rss/emacs.xml"        . emacs)
			   ("rss/debian.xml"       . debian)
			   ("rss/postgresqlfr.xml" . postgresqlfr)
			   ("rss/index.xml"        . nil))
  "An alist of RSS filename and subscribed TAG, nil meaning all")

;;;
;;; Some tools
;;;

;; my try at walk-path
(defun walk-path (path fun &optional
		       match-regexp depth-first filter filter-call depth)
  "walk given path recursively, calling fun for each entry

If filter is not nil it's expected to be a function taking
filename, attributes and depth as parameters, returning a
boolean. The (sub)path is walked into only when true.

When filter-call is true, we decide whether to call fun depending
on filter.
"
  (dolist (e (directory-files-and-attributes path t match-regexp))
    (let* ((filename   (car e))
	   (attributes (cdr e))
	   (is-subdir  (file-directory-p filename))
	   (cur-depth  (or depth 0))
	   (walk       (and is-subdir
			    ;; skip . and .. to protect the recursion
			    (not (string-match "/\\.\\.?$" filename))
			    (if (functionp filter)
				(funcall filter filename attributes depth)
			      t))))
      (when (and walk depth-first)
	(walk-path filename fun match-regexp depth-first filter
		   filter-call (1+ cur-depth)))

      (when (or (not is-subdir) (not filter-call) (and filter-call walk))
	(funcall fun filename attributes))

      (when (and walk (not depth-first))
	(walk-path filename fun match-regexp depth-first filter
		   filter-call (1+ cur-depth))))))
;;;
;;;  Tapoueh.org
;;;
(defun tapoueh-path-to-root ()
  "Return as many \"../\" as necessary to get back to website root"
  (let* ((current (muse-current-file))
	 (cwd     (file-name-directory current))
	 (project (muse-project-of-file current))
	 (root    (muse-style-element :path (caddr project))))
    (mapconcat (lambda (x) "../")
	       (cdr (split-string (file-relative-name current root) "/")) "")))

(defun tapoueh-style-sheet ()
  "Get the :style-sheet property and rework the link to the CSS"
  (let* ((current muse-publishing-current-output-path)
	 (cwd     (file-name-directory current))
	 (project (muse-project-of-file current))
	 (css     (muse-style-element :style-sheet (caddr project))))
    (replace-regexp-in-string "static" (concat (tapoueh-path-to-root) "static") css)))

(defun tapoueh-rss-index-href ()
  "Get the relative link to the RSS feed"
  (let* ((current muse-publishing-current-output-path)
	 (cwd     (file-name-directory current))
	 (project (muse-project-of-file current)))
    (concat "href=\"" (tapoueh-path-to-root) "rss/index.xml" "\"")))

(defun tapoueh-contents ()
  "Output the link to the /contents.html page"
  (concat (tapoueh-path-to-root) "contents.html"))

(defun tapoueh-root-index ()
  "Output the link to the /contents.html page"
  (concat (tapoueh-path-to-root) "index.html"))

(defun tapoueh-2ndquadrant-logo ()
  "Get the :style-sheet property and rework the link to the CSS"
  (concat "<img src=\""
	  (tapoueh-path-to-root)
	  "static/2ndQuadrant-cross.png\">"))

(defun tapoueh-current-page-url ()
  "Get the current page full URL"
  (let* ((current (muse-current-file))
	 (cwd     (file-name-directory current))
	 (project (muse-project-of-file current))
	 (base    (muse-style-element :base-url (caddr project)))
	 (root    (muse-style-element :path (caddr project)))
	 (link    (concat (file-name-sans-extension
			   (file-relative-name current root))
			  ".html")))
    (concat base link)))

(defun tapoueh-extract-buffer-directive (directive)
  "Extracts DIRECTIVE content from current buffer."
  (save-excursion
    (beginning-of-buffer)
    (when (search-forward (concat "#" directive) nil t)
      (while (looking-at " ") (forward-char))
      (buffer-substring-no-properties (point)
				      (muse-line-end-position)))))

(defun tapoueh-extract-file-directive (file directive)
  "Extracts DIRECTIVE content from the source of Muse FILE."
  (with-temp-buffer
    (muse-insert-file-contents file)
    (tapoueh-extract-buffer-directive directive)))

(defun tapoueh-extract-directive (&optional directive file)
  "Extracts tags from the source of Muse FILE, return a list of
them.  When FILE is omitted, extract tags from current buffer"
  (let ((value (if file (tapoueh-extract-file-directive file directive)
		 (tapoueh-extract-buffer-directive directive))))
    (if (and (string= directive "tags") value)
	(split-string value " ")
      value)))

(defun tapoueh-insert-article-link (link title date)
  "Insert a link to the current buffer at the current position"
  ;; don't forget to prepend the link with ../, we're in tags/ subdir
  (insert (format "\n[[../%s][%s]] " link title)
	  (format "<class name=\"date\">%s</class>\n\n" date)))

;;;
;;; TAG files need to be sorted by date, recent first
;;;
(defun tapoueh-goto-next-article-in-buffer ()
  "Move point in current buffer to the beginning of the line
where the next article link is, and return the article's date.
Only intended for Muse buffers containing article lists, like the
tags index files"
  (let ((date
	 (when (search-forward "]] <class name=\"date\">" nil t)
	   (buffer-substring-no-properties
	    (point) (- (search-forward "</class>") 8)))))
    (beginning-of-line)
    date))

(defun tapoueh-add-article-to-tag (link title date tag)
  "Add an article entry (link) in the TAG file"
  (let* ((tagfile (concat root "/tags/" (downcase tag) ".muse"))
	 (reftime (tapoueh-parse-date date)))
    (message "tapoueh-add-article-to-tag: %S" tagfile)
    (if (file-exists-p tagfile)
	(with-current-buffer (find-file-noselect tagfile)
	  (beginning-of-buffer)
	  (unless (search-forward link nil t)
	    (while (time-less-p reftime
				(or (tapoueh-parse-date
				     (tapoueh-goto-next-article-in-buffer))
				    (encode-time 0 0 0 0 0 0)))
	      (forward-line))
	    (open-line 2)
	    (tapoueh-insert-article-link link title date)
	    (save-buffer)))
      ;; new tagfile
      (with-temp-buffer
	(insert (format "#title Articles Tagged %s\n\n" tag)
		(format "* Articles Tagged %s\n\n" tag))
	(tapoueh-insert-article-link link title date)
	(write-file tagfile)))))

(defun tapoueh-add-article-to-tags-list (title date tags)
  "For each root/tags/<tag>.muse file, add an entry to given article"
  (let* ((current (muse-current-file))
	 (project (muse-project-of-file current))
	 (root    (muse-style-element :path (caddr project)))
	 (link    (concat (file-name-sans-extension
			   (file-relative-name current root))
			  ".html")))
    (loop for tag in tags
	  do (tapoueh-add-article-to-tag link title date tag))))

(defun tapoueh-add-tag-links ()
  "Muse Style :before function, add a tags section to the article"
  (let ((tags  (tapoueh-extract-directive "tags"))
	(title (tapoueh-extract-directive "title"))
	(date  (tapoueh-format-date (tapoueh-extract-directive "date")))
	(updir (tapoueh-path-to-root)))
    (message "tapoueh-add-tag-links: %S" (muse-current-file))
    (when tags
      (save-excursion
	(end-of-buffer)
	(insert "\n\n* Tags\n\n")
	(loop for tag in tags
	      do (insert (format "[[%stags/%s.html][%s]] "
				 updir (downcase tag) tag))))
      (tapoueh-add-article-to-tags-list title date tags))))

;;;
;;; Custom date parsing
;;;
(defun tapoueh-parse-date (date-string)
  "date is written as YYYYMMDD-HH:MM or YYYYMMDD, return elisp time value"

  (cond
   ((null date-string)
    nil)

   ;; "20110512-10:30"
   ((string-match (rx bol (repeat 8 (any "0-9"))) date-string)

    (let* ((l     (length date-string))
	   (year  (string-to-number (substring date-string 0 4)))
	   (month (string-to-number (substring date-string 4 6)))
	   (day   (string-to-number (substring date-string 6 8)))
	   (hour  (if (> l 8) (string-to-number (substring date-string 9 11)) 0))
	   (min   (if (> l 8) (string-to-number (substring date-string 12 14)) 0)))
      (encode-time 0 min hour day month year)))

   ;; "Thursday, May 12 2011, 10:30"
   ((string-match (rx word space (repeat 2 digit) space (repeat 4 digit)) date-string)

    (let* ((split  (split-string date-string ","))
	   (date   (split-string (nth 1 split)))
	   (time   (when (eq 3 (length split))
		     (split-string (nth 2 split) ":")))
	   (hour   (if time (string-to-number (nth 0 time)) 0))
	   (min    (if time (string-to-number (nth 1 time)) 0))
	   (months ["January" "February" "March" "April" "May" "June" "July"
		    "August" "September" "October" "November" "December"])
	   (month  (1+ (position (nth 0 date) months :test 'string=)))
	   (day    (string-to-number (nth 1 date)))
	   (year   (string-to-number (nth 2 date))))
      (encode-time 0 min hour day month year)))

   (t nil)))

(defun tapoueh-format-date (date-string)
  "date is written as YYYYMMDD-HH:MM or YYYYMMDD, reformat for human printing"
  (when date-string
    (let* ((time  (tapoueh-parse-date date-string))
	   (dec   (decode-time time))
	   (hour  (nth 2 dec))
	   (min   (nth 1 dec)))
      (if (and (eq 0 hour) (eq 0 min))
	  (format-time-string "%A, %B %d %Y" time)
	(format-time-string "%A, %B %d %Y, %R" time)))))

;;;
;;; Add date to current article
;;;
;;;  <lisp>(tapoueh-insert-article-date-here)</lisp>
;;;
(defun tapoueh-insert-article-date-here ()
  "Insert the DATE directive, if any, properly formated, just here"
  (let ((date
	 (tapoueh-format-date
	  (tapoueh-extract-directive "date" (muse-current-file)))))
    (when date
      (insert date))))
;;;
;;; We want to see the article date in the output, which is not the default
;;; for the HTML Muse Style.
;;;
(defun tapoueh-set-coding-system ()
  ":after function to force the coding system to UTF-8"
  ;; from Muse documentation, :after is the place where to set the coding
  ;; system of the articles --- we WANT all of them to be utf-8
  (setq buffer-file-coding-system 'utf-8))

;;;
;;; Walk recursively through a directory and build a list of Muse articles
;;; found there in.
;;;
(defun tapoueh-walk-articles (&optional root subdirs-only)
  "Run through all subdirs from current page and list articles

Returns a list of (SOURCE LINK TITLE DATE FORMATED-DATE (TAG TAG))"
  (let ((cwd
	 (or root
	     (file-name-directory muse-publishing-current-output-path)))
	pages)
    (walk-path
     cwd
     (lambda (f a)
       (when (or
	      (and (file-directory-p f)
		   (not (member (file-name-nondirectory f) '("." ".."))))
	      (string-match ".muse$" (file-name-nondirectory f)))

	 (let ((relative (file-relative-name f cwd)))
	   (unless (and subdirs-only
			(not (file-directory-p f))
			(string= relative (file-name-nondirectory relative)))
	     (if (file-directory-p f)
		 (push (list f
			     (format "%s/index.html" relative)
			     (file-name-nondirectory f)
			     nil
			     nil
			     nil)
		       pages)

	       (let* ((html  (format "%s.html" (file-name-sans-extension relative)))
		      (title (tapoueh-extract-directive "title" f))
		      (date  (tapoueh-extract-directive "date" f))
		      (tags  (tapoueh-extract-directive "tags" f)))
		 (push (list f html title date (tapoueh-format-date date) tags)
		       pages))))))))
    (reverse pages)))

;;;
;;; INDEX Support
;;;
(defun tapoueh-insert-article-link-li (source link title date fdate tags)
  "insert given article in a nested list"
  (let* ((isdir    (and (> (length link) 10)
			(string= (substring link -10) "index.html")))
	 (spaces   (apply
		    'concat
		    (split-string
		     (replace-regexp-in-string "[^/]+" " " link) "/")))
	 (nesting  (if isdir spaces (concat " " spaces))))
    (if (and link (file-exists-p source))
	(insert (format "%s- [[%s][%s]]" nesting link title))
      (insert (format "%s- %s" nesting title)))
    (when date
      (insert (format " <class name=\"date\">%s</class>" fdate)))
    (insert "\n")))

(defun tapoueh-list-blog-articles (&optional subdirs-only no-index root)
  "Run through all subdirs from current page and list pages"
  (loop for (src link title date fdate tags) in (tapoueh-walk-articles root subdirs-only)
	unless (or (string-match ".git" src)
		   (and no-index
			(string= (file-name-nondirectory src) "index.muse")))
	do (tapoueh-insert-article-link-li src link title date fdate tags)))

;;;
;;; TAGS Support
;;;
(defun tapoueh-list-all-tags (&optional root)
  "Walk subdirs and list all found tags"
  (let ((tag-counts
	 (loop with counts
	       for (src link title date fdate tags) in (tapoueh-walk-articles root)
	       do (mapc (lambda (x) (let ((c (assoc x counts)))
				      (if c (setcdr c (1+ (cdr c)))
					(push (cons x 1) counts))))
			tags)
	       finally return counts)))
    (sort tag-counts (lambda (a b) (> (cdr a) (cdr b))))))

(defun tapoueh-tags-cloud (&optional subdir)
  "Produce a tag cloud from articles"
  (let* ((current muse-publishing-current-output-path)
	 (project (muse-project-of-file current))
	 (root    (muse-style-element :path (caddr project)))
	 (dir     (if subdir (concat root "/" subdir) root))
	 (tags    (tapoueh-list-all-tags dir))
	 (max     (when tags (float (cdar tags)))))
    (loop for (tag . count) in tags
	  do (insert
	      (format
	       (concat
		"<literal><a href=%stags/%s.html "
		"style=\"font-size: %2.2f%%;\">%s</a></literal> ")
	       (tapoueh-path-to-root)
	       (downcase tag)
	       (* 250 (/ count max))
	       tag)))))

;;;
;;; Mainly useful when importing a lot old untagged articles
;;;
(defun tapoueh-get-tag-list (content)
  "return a list of tags that matches given string"
  (loop for tag in '("PostgreSQL" "Emacs" "Muse" "debian" "pgcon" "Conferences"
		     "el-get" "Extensions" "release" "FOSDEM" "ip4r"
		     "backports" "modeline" "catalogs" "plpgsql" "skytools"
		     "backup" "restore" "pg_staging" "prefix" "preprepare"
		     "switch-window" "pgloader" "cssh" "mailq" "postfix" "rcirc"
		     "9.1" "pgsql-linum-format" "emacs-kicker")
	when (string-match tag content)
	collect tag))

(defun tapoueh-auto-tag-article ()
  "Auto collect tags from current buffer"
  (interactive)
  (insert (mapconcat 'identity (tapoueh-get-tag-list (buffer-string)) " ")))

;;;
;;; TAIL Support (most recent articles, recent first)
;;;
(defun tapoueh-article-date-less-p (a b)
  "Return t only when article a's date is time-less-p b's date.

An article is a list of SOURCE LINK TITLE DATE FORMATED-DATE TAGS"
  (time-less-p (or (tapoueh-parse-date (nth 3 a))
		   (encode-time 0 0 0 0 0 0))
	       (or (tapoueh-parse-date (nth 3 b))
		   (encode-time 0 0 0 0 0 0))))

(defun tapoueh-latest-articles (&optional n root)
  "List the N most recent articles found in ROOT"
  (let* ((articles (tapoueh-walk-articles root))
	 (sorted   (nreverse (sort articles #'tapoueh-article-date-less-p))))
    (loop for a in sorted
	  when (nth 3 a)
	  collect a into result
	  until (eq (length result) n)
	  finally return result)))

(defun tapoueh-insert-latest-articles (&optional n dir)
  "Insert the N most recent articles found in ROOT"
  (let* ((project (muse-project-of-file (muse-current-file)))
	 (root    (muse-style-element :path (caddr project)))
	 (cwd     (file-name-directory muse-publishing-current-output-path))
	 (d       (file-name-as-directory
		   (if dir (expand-file-name dir root) root)))
	 (updir   (unless (string= cwd d) (file-relative-name d cwd))))
    (loop for (src link title date fdate tags) in (tapoueh-latest-articles n d)
	  do (tapoueh-insert-article-link-li
	      src (concat updir link) title date fdate tags))))

;;;
;;; Breadcrumb support
;;;
;;;  <lisp>(tapoueh-insert-breadcrumb-here)</lisp>
;;;
(defun tapoueh-breadcrumb-to-current-page ()
  "Return a list of (name . link) from the index root page to current one"
  (let* ((current (muse-current-file))
	 (cwd     (file-name-directory current))
	 (project (muse-project-of-file current))
	 (root    (muse-style-element :path (caddr project)))
	 (path    (tapoueh-path-to-root))
	 (dirs    (split-string (file-relative-name current root) "/")))
    ;; ("blog" "2011" "07" "13-back-from-char11.muse")
    (append
     (list (cons "/dev/dim" (concat path "index.html")))
     (loop for p in (butlast dirs)
	   collect (cons p (format "%s%s/index.html" path p))
	   do (setq path (concat path p "/"))))))

(defun tapoueh-insert-breadcrumb-hrefs ()
  "The inserting of HTML links"
  (loop for (name . link) in (tapoueh-breadcrumb-to-current-page)
	do (insert (format "<a href=%s>%s</a>" link name) " / ")))

(defun tapoueh-insert-breadcrumb-here ()
  "Called from the template file (header or Muse file)"
  ;; (when (tapoueh-extract-directive "author" (muse-current-file))
    (tapoueh-insert-breadcrumb-hrefs))

;;;
;;; RSS Support
;;;
(defun tapoueh-get-output-rss-content ()
  "Current buffer is the HTML rendered article, extract the
content to place in the RSS item description"
  (beginning-of-buffer)
  (re-search-forward "<div id=\"article\">" nil t)
  (while (looking-at (rx space))	; we skip any white space
    (forward-char))
  (buffer-substring-no-properties
	    (point)
	    (progn
	      (re-search-forward "<h2>Tags</h2>" nil t)
	      (forward-line -1)
	      (while (looking-at (rx (or " " "\n"))) (forward-char -1))
	      (forward-char 1)
	      (point))))

(defun tapoueh-goto-next-rss-item ()
  "Move to next item, place the point at the beginning of the
opening line, and return the item's pubDate"
  ;; <pubDate>Mon, 31 Jan 2011 16:31:00 +0100</pubDate>
  (let ((date
	 ;; this function moves the point back to the opening <item> line,
	 ;; so we need to search twice to go to next entry (unless we're
	 ;; doing the first search from the beginning of the buffer)
	 (when (search-forward "<pubdate>" nil t (if (bobp) 1 2))
	   (buffer-substring-no-properties
	    (point) (- (search-forward "</pubDate>") 10)))))
    (if date
	(progn
	  (re-search-backward "<item>" nil t)
	  (beginning-of-line)
	  (date-to-time date))
      ;; don't forget to place point at the right place
      (re-search-forward "</channel>" nil t)
      (beginning-of-line)
      (encode-time 0 0 0 0 0 0))))

(defun tapoueh-add-article-to-rss (rss source output link)
  "add an ITEM to the RSS file, take the content from OUTPUT ---
in fact, from the current buffer."
  (let* ((title   (tapoueh-extract-directive "title" source))
	 (desc    (tapoueh-get-output-rss-content))
	 (author  "dim@tapoueh.org (Dimitri Fontaine)")
	 (date    (tapoueh-parse-date
		   (tapoueh-extract-directive "date" source)))
	 (pubdate (format-time-string "%a, %d %b %Y %T %z" date)))
    (with-current-buffer (find-file-noselect rss)
      (beginning-of-buffer)
      (when (search-forward link nil t)
	;; first remove the existing article
	(re-search-backward "<item>" nil t)
	(beginning-of-line)
	(delete-region (point)
		       (1+ (re-search-forward "</item>" nil t))))
      ;; now add the link entry
      (beginning-of-buffer)
      (while (time-less-p date (tapoueh-goto-next-rss-item)))
      ;; (open-line 2)
      (insert "<item>\n"
	      (format "  <title>%s</title>\n"
		      (replace-regexp-in-string "&" "&amp;" title))
	      (format "  <link>%s</link>\n" link)
	      (format "  <description><![CDATA[%s\n]]></description>\n" desc)
	      (format "  <author>%s</author>\n" author)
	      (format "  <pubDate>%s</pubDate>\n" pubdate)
	      (format "  <guid isPermaLink=\"true\">%s</guid>\n" link)
	      "</item>\n")
      (save-buffer))))

(defun tapoueh-add-item-to-rss (source output second-stage)
  ":final function used to publish the current blog entry into
the relevant RSS streams, if any. We consider the TAGs of the
file to be the relevant information."
  (let* ((project (muse-project-of-file source))
	 (root    (muse-style-element :path (caddr project)))
	 (url     (muse-style-element :base-url (caddr project)))
	 (relname (file-relative-name output root))
	 (link    (concat url relname))
	 (tags    (mapcar (lambda (x) (intern (downcase x)))
			  (tapoueh-extract-directive "tags" source))))
    (when tags
      (loop for (rss . tag) in tapoueh-rss-tags
	    when (or (null tag) (member tag tags))
	    do (tapoueh-add-article-to-rss
		(concat root "/" rss) source output link)))))

;;;
;;; Social Networks Integration
;;;
(defun tapoueh-social-div ()
  "Output a <div> for connecting to social network for tagged articles"
  (when (tapoueh-extract-directive "tags" (muse-current-file))
    (insert
     "<div id=\"social\">\n"
     "<ul>"
     ;; Google +1
     "<li><g:plusone size=\"tall\" href=\""
     (tapoueh-current-page-url)
     "\"></g:plusone></li>\n"
     ;; Twitter this
     "<li><a href=\"http://twitter.com/share\" class=\"twitter-share-button\" data-count=\"vertical\">Tweet</a><script type=\"text/javascript\" src=\"http://platform.twitter.com/widgets.js\"></script></li>\n"
     "<li><iframe src=\"http://www.facebook.com/plugins/like.php?href="
     (tapoueh-current-page-url)
     ";layout=box_count;show_faces=false\"\n"
     "   scrolling=\"no\" frameborder=\"0\" allowTransparency=\"true\"\n"
     "   style=\"border:none; overflow:hidden;\"></iframe></li>\n"
     "</ul>\n"
     "</div>\n")))

;;;
;;; SiteMap
;;;
(defun tapoueh-sitemap (project)
  "Create the sitemap.xml file"
  (let ((root (muse-style-element :path (caddr project))))
    (with-temp-file (format "%s/sitemap.xml" root)
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
	      "<urlset\n"
	      "    xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\"\n"
	      "    xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n"
	      "    xsi:schemaLocation=\"http://www.sitemaps.org/schemas/sitemap/0.9\n"
	      "        http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd\">\n")
      (loop for (src link title date fdate tags) in (tapoueh-walk-articles root)
	    unless (string-match ".git" src)
	    do (progn
		 (insert "<url>\n"
			 (format "  <loc>http://tapoueh.org/%s</loc>\n" link)
			 (format "  <lastmod>%s</lastmod>\n"
				 (format-time-string "%Y%d%m-%R"
						     (nth 4 (file-attributes src))))
			 (format "  <changefreq>%s</changefreq>\n"
				 (cond ((string-match (rx (or "tag" "rss")) link)
					"always")
				       ((string-match "index" link)
					"always")
				       (t "weekly")))
			 "</url>\n")))
      (insert "</urlset>\n"))))

(add-hook 'muse-after-project-publish-hook 'tapoueh-sitemap)

;;;
;;; Facilities
;;;
(defun tapoueh-insert-muse-headers ()
  "insert headers in a new Muse file"
  (interactive)
  (let* ((project (muse-project-of-file (muse-current-file)))
	 (pname   (car project)))
    (when (string= pname "tapoueh.org")
	(beginning-of-buffer)
	(unless (or (and (boundp 'muse-publishing-current-file)
			 muse-publishing-current-file)
		    (and (buffer-file-name)
		     (file-exists-p (buffer-file-name))
		     (tapoueh-extract-directive "title" (muse-current-file))))
	  (message "New Muse File: %s" (muse-current-file))
	  (let ((title (read-string "Article Title: ")))
	    (insert "#author Dimitri Fontaine\n"
		    (format "#title  %s\n" title)
		    (format "#date   %s\n" (format-time-string "%Y%m%d-%R"))
		    "#tags   \n"
		    "\n"))))))

(add-hook 'muse-mode-hook 'tapoueh-insert-muse-headers)

;;;
;;; C-c C-r to rsync the static website to the hosting server
;;;
(defvar dim:muse-rsync-options "-avz"
  "rsync options")

(defvar dim:muse-rsync-source "~/dev/tapoueh.org"
  "local path from where to rsync, with no ending /")

(defvar dim:muse-rsync-target
  "dim@tapoueh.org:/var/www/blog.tapoueh.org"
  "Remote URL to use as rsync target, with no ending /")

(defun dim:muse-project-rsync ()
  "publish tapoueh.org using rsync"
  (interactive)
  (let* ((rsync-command (format "rsync %s %s %s"
				dim:muse-rsync-options
				(concat dim:muse-rsync-source "/")
				(concat dim:muse-rsync-target "/"))))
    (with-current-buffer (get-buffer-create "*muse-rsync*")
      (erase-buffer)
      (insert (concat rsync-command "\n"))
      (message "%s" rsync-command)
      (insert (shell-command-to-string rsync-command))
      (insert "\n"))))

(define-key muse-mode-map (kbd "C-c C-r") 'dim:muse-project-rsync)
