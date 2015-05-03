;;;; muse-parser.lisp
;;;
;;; Parse the Emacs Muse format from the Muse project, as found at:
;;;
;;;   http://mwolson.org/projects/EmacsMuse.html

(in-package #:tapoueh)

(defparameter *include-src-lang* nil
  "Do we want to include :class \"lang\" markup in code blocks.")

(def-suite muse :description "Emacs Muse Parser Test Suite.")
(in-suite muse)

;;
;; Now the Parsing, with first some basics
;;
(defun not-newline (char)
  (not (eql #\newline char)))

(defun not-newline-or-special (char)
  (not (member char (list #\Newline #\Tab #\[ #\] #\< #\> #\= #\*))))

(defrule whitespace (or #\space #\newline #\linefeed))
(defrule whitespaces (* whitespace))

(defrule empty-line #\newline
  (:constant nil))

(defrule non-empty-line (and (+ (not-newline-or-special character))
			     (? #\newline))
  (:lambda (source)
    (destructuring-bind (text newline) source
      (declare (ignore newline))
      (text text))))

(defrule line (or empty-line non-empty-line)
  (:identity t))

(defrule trimmed-line line
  (:lambda (line)
    (string-trim '(#\space #\tab) line)))

;;
;; Directives
;;
(defrule name (+ (alpha-char-p character))
  (:text t))

(defrule directive-author (and "#author" (+ (or #\Tab #\Space)))
  (:constant :author))

(defrule directive-title (and "#title" (+ (or #\Tab #\Space)))
  (:constant :title))

(defrule directive-date (and "#date" (+ (or #\Tab #\Space)))
  (:constant :date))

(defrule directive-tags (and "#tags" (+ (or #\Tab #\Space)))
  (:constant :tags))

(defrule directive-desc (and "#desc" (+ (or #\Tab #\Space)))
  (:constant :desc))

(defrule directive-city (and "#city" (+ (or #\Tab #\Space)))
  (:constant :city))

(defrule directive-name (or directive-author
			    directive-title
			    directive-date
			    directive-city
			    directive-tags
			    directive-desc))

(defrule directive (and directive-name non-empty-line)
  (:lambda (source)
    (destructuring-bind (name value) source
      (if (eq name :tags)
	  (list name (split-sequence #\Space value))
	  (list name value)))))

(defrule directives (and (+ directive) empty-line)
  (:lambda (source)
    (destructuring-bind (directives e) source
      (declare (ignore e))
      ;; build a plist of the directives, use that to create a struct
      (apply #'make-muse-article (apply #'append directives)))))

#+5am
(test parse-directive
      "Test parsing some directive"
      (is (equal (parse 'directive "#author dim") '(:author "dim")))
      (is (equal (parse 'directive "#author	dim") '(:author "dim")))
      (is (equal (parse 'directive "#title   plop") '(:title "plop")))
      (is (tree-equal
	   (parse 'directive "#tags   a b c")
	   '(:tags ("a" "b" "c"))
	   :test #'equal)))

#+5am
(test parse-directives
      "Test parsing directives"
      (is (same-document-p
	   (parse 'directives "#author Dimitri Fontaine
#title  from Parsing to Compiling
#date   20130513-11:08
#tags   Common-Lisp Parser Emacs Muse

")
	   (make-muse-article
	    :author "Dimitri Fontaine"
	    :title "from Parsing to Compiling"
	    :date "20130513-11:08"
	    :tags '("Common-Lisp" "Parser" "Emacs" "Muse")))))

;;
;; Document content
;;
(defrule word (+ (not (or whitespace #\Tab #\[ #\] #\< #\> #\= #\*)))
  (:text t))

(defrule words (+ (or word #\Space #\Tab))
  (:lambda (source)
    (apply #'concatenate 'string source)))

(defrule link-part (and "[" (+ (not "]")) "]")
  (:lambda (source)
    (destructuring-bind (open target close) source
      (declare (ignore open close))
      (text target))))

(defun image-filename-p (maybe-filename)
  "Return non-nil only when MAYBE-FILENAME has a pathname-type of an image"
  (member (pathname-type maybe-filename) *image-type-list* :test #'string=))

(defun maybe-image-tagify (string &key as-link)
  "Turn STRING into (:img :src STRING) if it's a local image filename."
  (if (and string
	   (not (ppcre:all-matches "^http(s?)://" string))
	   (image-filename-p string))
      `(:img :src ,string)
      (if as-link
	  `(:a :href ,string ,string)
	  string)))

(defrule link (and "[" link-part (? link-part) "]")
  (:lambda (source)
    (destructuring-bind (open target label close) source
      (declare (ignore open close))
      (if label
	  `(:a :href ,target ,(maybe-image-tagify label :as-link nil))
	  (maybe-image-tagify target :as-link t)))))

#+5am
(test parse-link
      "Test parsing links"
      (is (tree-equal
	   (parse 'link "[[../../../pgsql/pgloader.html][pgloader]]")
	   '(:A :HREF "../../../pgsql/pgloader.html" "pgloader")
	   :test #'equalp))
      (is (tree-equal
	   (parse 'link "[[../../../images/lightbulb.gif]]")
	   '(:IMG :SRC "../../../images/lightbulb.gif")
	   :test #'equalp))
      (is (tree-equal
	   (parse 'link "[[https://groups.google.com/forum/?fromgroups=#!topic/comp.lang.lisp/JJxTBqf7scU][What is symbolic compoutation?]]")
	   '(:A :HREF
	     "https://groups.google.com/forum/?fromgroups=#!topic/comp.lang.lisp/JJxTBqf7scU"
	     "What is symbolic compoutation?")
	   :test #'equalp))
      (is (tree-equal
	   (parse 'link "[[skytools.html#slony][What does londiste lack that slony has?]]")
	   '(:A :HREF "skytools.html#slony" "What does londiste lack that slony has?")
	   :test #'equalp))
      (is (tree-equal
	   (parse 'link "[[http://forum.ubuntu-fr.org/viewtopic.php?id=218883]]")
	   '(:A :HREF "http://forum.ubuntu-fr.org/viewtopic.php?id=218883"
	     "http://forum.ubuntu-fr.org/viewtopic.php?id=218883")
	   :test #'equalp))
      (is (tree-equal
	   (parse 'link "[[../../../images/confs/the_need_for_speed.pdf][../../../images/confs/the_need_for_speed-3.png]]")
	   '(:a :href "../../../images/confs/the_need_for_speed.pdf"
	     (:img :src "../../../images/confs/the_need_for_speed-3.png"))
	   :test #'equalp))
      (is (tree-equal
	   (parse 'link "[[http://commons.wikimedia.org/wiki/File:BlankMap-World-2009.PNG]]")
	   '(:A :HREF "http://commons.wikimedia.org/wiki/File:BlankMap-World-2009.PNG"
	     "http://commons.wikimedia.org/wiki/File:BlankMap-World-2009.PNG")
	   :test #'equalp)))

(defrule attr
    (and whitespaces
	 word
	 "="
	 (? #\")
	 (+ (or #\Space #\- #\. (alphanumericp character)))
	 (? #\"))
  (:lambda (source)
    (destructuring-bind (ws name eq lq value rq) source
      (declare (ignore ws eq lq rq))
      (list (read-from-string (format nil ":~a" name)) (text value)))))

(defrule attrs (+ attr)
  (:lambda (source)
    (apply #'append source)))

(defrule code (and "<code" (? attrs) ">" (+ (not "</code>")) "</code>")
  (:lambda (source)
    (destructuring-bind (open attrs gt code close) source
      (declare (ignore open close gt))
      `(:code ,@attrs ,(who:escape-string (text code))))))

(defrule monospace (and #\= (+ (not "=")) #\=)
  (:lambda (source)
    (destructuring-bind (open content close) source
      (declare (ignore open close))
      `(:code ,(who:escape-string (text content))))))

(defrule italics (and #\* (+ (or link monospace words #\Newline)) #\*)
  (:lambda (source)
    (destructuring-bind (open content close) source
      (declare (ignore open close))
      `(:em ,@content))))

(defrule bold (and #\* #\*
		   (+ (or link monospace words #\Newline))
		   #\* #\*)
  (:lambda (source)
    (destructuring-bind (open1 open2 content close1 close2) source
      (declare (ignore open1 open2 close1 close2))
      `(:strong ,@content))))

(defrule heavy (and #\* #\* #\*
		    (+ (or link monospace words #\Newline))
		    #\* #\* #\*)
  (:lambda (source)
    (destructuring-bind (o1 o2 o3 content c1 c2 c3) source
      (declare (ignore o1 o2 o3 c1 c2 c3))
      `(:em (:strong ,@content)))))

#+5am
(test parse-emphasis
      "Test *italics* and **bold** and ***heavy*** etc."
      (is (equalp (parse 'monospace "=code=")
		  '(:code "code")))
      (is (equalp (parse 'monospace "=@>=")
		  '(:code "@>")))
      (is (equalp (parse 'code "<code>=</code>")
		  '(:code "=")))
      (is (equalp
	   (parse 'code "<code src=\"sql\">SELECT colname FROM table WHERE pk = 1234;</code>")

	   '(:code :SRC "sql" "SELECT colname FROM table WHERE pk = 1234;")))
      (is (equalp (parse 'italics "*some italic words*")
		  '(:em  "some italic words")))
      (is (equalp (parse 'italics "*An edited version of =hstore--1.1.sql= for vertical space concerns*")
		  '(:EM "An edited version of " (:code "hstore--1.1.sql")
		    " for vertical space concerns")))
      (is (equalp (parse 'bold "**this is bold**")
		  '(:strong "this is bold")))
      (is (equalp (parse 'heavy "***this is heavy***")
		  '(:em (:strong "this is heavy")))))

(defrule centered (and #\Tab (* (or #\Tab whitespace))
		       (+ (or heavy bold italics monospace code link words))
		       (? #\Newline))
  (:lambda (source)
    (destructuring-bind (tab ws thing nl) source
      (declare (ignore tab ws nl))
      `(:center ,@thing))))

#+5am
(test parse-centered
      "Test some centered (tabulated) contents"
      (is (equalp (parse 'centered "	*ahah*")
		  '(:center (:em  "ahah"))))
      (is (equalp (parse 'centered "	*ahah*
")
		  '(:center (:em  "ahah"))))
      (is (equalp (parse 'centered "		       [[http://postgresqlrussia.org/articles/view/131][../../../images/Moskva_DB_Tools.v3.png]]")
		  '(:CENTER
		    (:A :HREF "http://postgresqlrussia.org/articles/view/131"
		     (:IMG :SRC "../../../images/Moskva_DB_Tools.v3.png")))))
      (is (equalp
	   (parse 'centered "	*Photo by [[http://www.sai.msu.su/~megera/][Oleg Bartunov]]*")
	   '(:CENTER
 (:EM "Photo by " (:A :HREF "http://www.sai.msu.su/~megera/" "Oleg Bartunov"))))))

(defrule title (and (+ #\*) (+ whitespace)
		    (* (or heavy bold italics monospace link non-empty-line)))
  (:lambda (source)
    (destructuring-bind (stars ws rest) source
      (declare (ignore ws))
      (let ((title
	     (remove-if (lambda (c) (member c '(#\Newline #\Linefeed))) rest)))
	(case (length stars)
	  (1 `(:h1 ,@title))
	  (2 `(:h2 ,@title))
	  (3 `(:h3 ,@title))
	  (4 `(:h4 ,@title))
	  (5 `(:h5 ,@title))
	  (6 `(:h6 ,@title)))))))

#+5am
(test parse-title
      "Test parsing a title"
      (is (equal (parse 'title "** a title") '(:h2 "a title")))
      (is (equal (parse 'title "***   another  title
")
		 '(:h3 "another  title")))
      (is (equal (parse 'title "** =whois dim=")
		 '(:H2 (:SPAN :CLASS "tt" "whois dim")))))

(defun muse-lang-to-highlight-js-lang (lang)
  "Convert a Muse lang=... attribute to what highlight.js expects"
  (cond ((string= "common-lisp" lang) "lisp")
	(t lang)))

(defrule src (and "<src" (? attrs) ">" (+ (not "</src>")) "</src>")
  (:lambda (source)
    (destructuring-bind (open attrs gt source close) source
      (declare (ignore open gt close))
      (destructuring-bind (&key lang &allow-other-keys)
	  attrs
	(let ((lang
	       (when *include-src-lang*
		 `(:class ,(muse-lang-to-highlight-js-lang lang)))))
	  `(:pre
	    (:code ,@lang
		   ,(string-left-trim '(#\Newline #\Space #\Tab)
				      (who:escape-string (text source))))))))))

#+5am
(test parse-src
      "Test some <src>content</src>"
      (is (equalp
	   (parse 'src "<src lang=\"common-lisp\">your code snippet here</src>")
	   '(:PRE (:code "your code snippet here")))))

(defrule class (and "<class" (? attrs) ">" (+ (not "</class>")) "</class>")
  (:lambda (source)
    (destructuring-bind (open attrs gt content close) source
      (declare (ignore open close gt))
      `(:class ,@attrs ,(text content)))))

#+5am
(test parse-class
      "Test some <class>content</class>"
      (is (equalp (parse 'class "<class name=\"hack\"> </class>")
		  '(:CLASS :NAME "hack" " "))))

(defrule literal (and "<literal>" (+ (not "</literal>")) "</literal>")
  (:lambda (source)
    (destructuring-bind (open text close) source
      (declare (ignore open close))
      (text text))))

#+5am
(test parse-literal
      "Test some <literal>content</literal>"
      (is (equalp (parse 'literal "<literal>some content here</literal>")
		  '"some content here"))
      (is (equalp
	   (parse 'literal "<literal><div class=\"plop\">hey</div></literal>")
	   "<div class=\"plop\">hey</div>")))

(defrule quote (and "<quote" (? attrs) ">" (+ (or p src empty-line)) "</quote>")
  (:lambda (source)
    (destructuring-bind (quote attrs gt content unquote) source
      (declare (ignore quote gt unquote))
      (destructuring-bind (&key author title &allow-other-keys) attrs
	`(:blockquote
	  ,@(remove-if #'null content)
	  ,(when author
		 `(:small
		   ,author
		   ,@(when title
			   (list " in " `(:cite :title ,title ,title))))))))))

#+5am
(test parse-quote
      "Test some <quote>content</quote>"
      (is (equalp (parse 'quote "<quote>some content here</quote>")
		  '(:blockquote (:p "some content here"))))
      (is (equalp (parse 'quote "<quote>
<src lang=\"sql\">
SELECT * FROM planet.postgresql.org WHERE author = \"dim\";
</src>
</quote>")
		  '(:BLOCKQUOTE
		    (:PRE "
SELECT * FROM planet.postgresql.org WHERE author = \"dim\";
"))
		  )))

(defrule lisp (and "<lisp>" (+ (not "</lisp>")) "</lisp>")
  (:lambda (source)
    (eval-lisp-tags (text source))))

#+5am
(test parse-lisp
      "Test some <lisp>(code)</lisp>"
      (is (equalp (parse 'lisp "<lisp>(tapoueh-style-sheet)</lisp>")
		  "<link rel='stylesheet' type='text/css' media='all' href='/static/styles.css' />")))

;;; FIXME, actually parse and include the file with the right properties
;;; <include file="generate-pgloader-config.sql" markup="src" lang="sql">
(defrule include (and "<include" (? attrs) ">")
  (:lambda (source)
    (destructuring-bind (include attrs gt) source
      (declare (ignore include gt))
      (destructuring-bind (&key file &allow-other-keys) attrs
	`(:pre (:code ,(who:escape-string
			(slurp-file-into-string
			 (merge-pathnames file *muse-parser-cwd*)))))))))

(defrule contents (and "<contents" (? attrs) ">")
  ;; ignore table of contents completely for now
  (:constant ""))

(defrule lines (+ non-empty-line)
  (:lambda (lines)
    (text (loop
	     for line in lines
	     append (list line #\Newline)))))

(defrule p (and (+ (or lines
		       (and (or heavy bold italics monospace code link)
			    (? #\Newline))))
		(? #\Newline))
  (:lambda (source)
    (destructuring-bind (p-list nl) source
      (declare (ignore nl))
      `(:p ,@(loop
		for p in p-list
		when (listp p) append p
		else collect p)))))

(defrule para (and p (* empty-line))
  (:lambda (source)
    (destructuring-bind (p e) source
      (declare (ignore e))
      p)))

(defrule list-text (+ (not-newline-or-special character))
  (:text t))

(defrule list-line (and #\Space #\- #\Space
			(+ (or heavy bold italics monospace code link list-text))
			#\Newline)
  (:lambda (source)
    (destructuring-bind (sp1 dash sp2 content nl) source
      (declare (ignore sp1 dash sp2 nl))
      `(:li ,@content))))

(defrule list (and (+ list-line) (? #\Newline))
  (:lambda (source)
    `(:ul ,@(car source))))

(defrule block (or title
		   centered
		   class
		   src
		   literal
		   include
		   quote
		   lisp
		   contents
		   list
		   para
		   empty-line))

(defrule body (* block)
  (:lambda (source)
    (remove-if #'null source)))

(defrule article (and directives body)
  (:lambda (source)
    (destructuring-bind (document content) source
      (setf (muse-contents document) content)
      document)))

;;
;; Intermediate Meta Data Parsing: article chapeau
;;
;; That's the directives and the first paragraph and the image following the
;; first paragraph, if any such.
;;
(defrule chapeau (and directives block block)
  (:lambda (source)
    (destructuring-bind (document first-para image) source
      (setf (muse-first-para document) first-para)
      (setf (muse-image document) (cadr image))	; drop the :center
      document)))

