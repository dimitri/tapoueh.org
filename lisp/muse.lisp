;;;; muse.lisp
;;;
;;; Parse the Emacs Muse format from the Muse project, as found at:
;;;
;;;   http://mwolson.org/projects/EmacsMuse.html

(in-package #:tapoueh)

(def-suite muse :description "Emacs Muse Parser Test Suite.")
(in-suite muse)

(defparameter *muse-pathname-type* "muse"
  "The file extension we use to store Muse files")

;;
;; Document data structure, pretty loose
;;
(defstruct
    (muse
      (:constructor make-muse)
      (:constructor make-muse-article (&key
				       author title date tags desc contents
				       &aux (timestamp (when date
							 (parse-date date))))))
  author title date timestamp tags desc contents)

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

(defmethod to-html ((document muse))
  "Produce the HTML of given Document"
  (eval `(with-html-output-to-string (s nil :indent t)
	   (declare (ignore s))
	   ,(muse-contents document))))

;;; a .muse file with the 4 directives is an "article"
(defmethod sort-articles ((a1 muse) (a2 muse)
			  &key
			    (test #'local-time:timestamp<)
			    (key 'timestamp))
  (let ((t1 (slot-value a1 key))
	(t2 (slot-value a2 key)))
    (when (and t1 t2)
      (funcall test t1 t2))))

(defun parse-muse-article (pathname)
  "Parse the Muse article at PATHNAME and return a muse structure."
  (parse 'article (slurp-file-into-string pathname)))

(defun parse-muse-directives (pathname)
  "Only parse the Muse directives, not the whole document"
  (or (parse 'directives (slurp-file-into-string pathname) :junk-allowed t)
      pathname))

(defun muse-file-type-p (pathname)
  "Returns a generalized boolean true when pathname extension is .muse"
  (string= (pathname-type pathname) *muse-pathname-type*))

;;
;; Now the Parsing, with first some basics
;;
(defun not-newline (char)
  (not (eql #\newline char)))

(defrule whitespace (or #\space #\newline #\linefeed))
(defrule whitespaces (* whitespace))

(defrule empty-line #\newline
  (:constant ""))

(defrule non-empty-line (and (+ (not-newline character)) (? #\newline))
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

(defrule directive-name (or directive-author
			    directive-title
			    directive-date
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
(defun muse-word-p (char)
  "Non decorated words, with spaces inside"
  (or (member char #.(quote (coerce "`~!@#$%^&()-_+{}\\|;:'\",./?" 'list)))
      (alphanumericp char)))

(defrule word (+ (not (or whitespace #\Tab #\[ #\] #\< #\> #\= #\*)))
  (:text t))

(defrule words (+ (or word whitespace))
  (:lambda (source)
    (apply #'concatenate 'string source)))

(defrule link-part (and "[" (+ (not "]")) "]")
  (:lambda (source)
    (destructuring-bind (open target close) source
      (declare (ignore open close))
      (text target))))

(defrule link (and "[" link-part (? link-part) "]")
  (:lambda (source)
    (destructuring-bind (open target label close) source
      (declare (ignore open close))
      (if label
	  `(:a :href ,target ,label)
	  `(:img :src ,target)))))

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
	   '(:IMG :SRC "http://forum.ubuntu-fr.org/viewtopic.php?id=218883")
	   :test #'equalp)))

(defrule attr
    (and whitespaces
	 word
	 "="
	 (? #\")
	 (+ (or #\- #\. (alphanumericp character)))
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
      (declare (ignore open close))
      `(:span :class "tt" ,@attrs ,(text code)))))

(defrule monospace (and #\= (+ (not "=")) #\=)
  (:lambda (source)
    (destructuring-bind (open content close) source
      (declare (ignore open close))
      `(:span :class "tt" ,(text content)))))

(defrule italics (and #\* (+ (or link monospace words)) #\*)
  (:lambda (source)
    (destructuring-bind (open content close) source
      (declare (ignore open close))
      `(:em ,@content))))

(defrule bold (and #\* #\* (+ (or link monospace words)) #\* #\*)
  (:lambda (source)
    (destructuring-bind (open1 open2 content close1 close2) source
      (declare (ignore open1 open2 close1 close2))
      `(:strong ,@content))))

(defrule heavy (and #\* #\* #\* (+ (or link monospace words)) #\* #\* #\*)
  (:lambda (source)
    (destructuring-bind (o1 o2 o3 content c1 c2 c3) source
      (declare (ignore o1 o2 o3 c1 c2 c3))
      `(:em (:strong ,@content)))))

#+5am
(test parse-emphasis
      "Test *italics* and **bold** and ***heavy*** etc."
      (is (equalp (parse 'monospace "=code=")
		  '(:span :class "tt" "code")))
      (is (equalp (parse 'monospace "=@>=")
		  '(:span :class "tt" "@>")))
      (is (equalp (parse 'code "<code>=</code>")
		  '(:span :class "tt" "=")))
      (is (equalp
	   (parse 'code "<code src=\"sql\">SELECT colname FROM table WHERE pk = 1234;</code>")

	   '(:SPAN :CLASS "tt" :SRC "sql" "SELECT colname FROM table WHERE pk = 1234;")))
      (is (equalp (parse 'italics "*some italic words*")
		  '(:em  "some italic words")))
      (is (equalp (parse 'italics "*An edited version of =hstore--1.1.sql= for vertical space concerns*")))
      '(:EM "An edited version of " (:SPAN :CLASS "tt" "hstore--1.1.sql")
	" for vertical space concerns")
      (is (equalp (parse 'bold "**this is bold**")
		  '(:strong "this is bold")))
      (is (equalp (parse 'heavy "***this is heavy***")
		  '(:em (:strong "this is heavy")))))

(defrule centered (and #\Tab (* (or #\Tab whitespace))
		       (or heavy bold italics monospace code link words)
		       (? #\Newline))
  (:lambda (source)
    (destructuring-bind (tab ws thing nl) source
      (declare (ignore tab ws nl))
      `(:center ,thing))))

#+5am
(test parse-centered
      "Test some centered (tabulated) contents"
      (is (equalp (parse 'centered "	*ahah*")
		  '(:center (:em  "ahah"))))
      (is (equalp (parse 'centered "	*ahah*
")
		  '(:center (:em  "ahah"))))
      (is (equalp (parse 'centered "		       [[http://postgresqlrussia.org/articles/view/131][../../../images/Moskva_DB_Tools.v3.png]]")
		  '(:center (:a :href "http://postgresqlrussia.org/articles/view/131"
			     "../../../images/Moskva_DB_Tools.v3.png"))))
      (is (equalp
	   (parse 'centered "	*Photo by [[http://www.sai.msu.su/~megera/][Oleg Bartunov]]*")
	   '(:CENTER
 (:EM "Photo by " (:A :HREF "http://www.sai.msu.su/~megera/" "Oleg Bartunov"))))))

(defrule title (and (+ #\*) whitespaces non-empty-line)
  (:lambda (source)
    (destructuring-bind (stars ws rest) source
      (declare (ignore ws))
      (let ((title
	     (remove-if (lambda (c) (member c '(#\Newline #\Linefeed))) rest)))
	(case (length stars)
	  (1 `(:h1 ,title))
	  (2 `(:h2 ,title))
	  (3 `(:h3 ,title))
	  (4 `(:h4 ,title))
	  (5 `(:h5 ,title))
	  (6 `(:h6 ,title)))))))

#+5am
(test parse-title
      "Test parsing a title"
      (is (equal (parse 'title "** a title") '(:h2 "a title")))
      (is (equal (parse 'title "***   another  title
")
		 '(:h3 "another  title"))))

(defrule src (and "<src" (? attrs) ">" (+ (not "</src>")) "</src>")
  (:lambda (source)
    (destructuring-bind (open attrs gt source close) source
      (declare (ignore open attrs gt close))
      `(:pre ,(text source)))))

#+5am
(test parse-src
      "Test some <src>content</src>"
      (is (equalp
	   (parse 'src "<src lang=\"common-lisp\">your code snippet here</src>")
	   '(:PRE "your code snippet here"))))

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

(defrule quote (and "<quote>" (+ (or paragraph src)) "</quote>")
  (:lambda (source)
    (destructuring-bind (open content close) source
      (declare (ignore open close))
      `(:blockquote ,@content))))

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
		  '(:BLOCKQUOTE (:P)
 (:PRE "
SELECT * FROM planet.postgresql.org WHERE author = \"dim\";
")
 (:P)))))

(defrule lisp (and "<lisp>" (+ (not "</lisp>")) "</lisp>")
  (:lambda (source)
    (destructuring-bind (open s-exprs close) source
      (declare (ignore open close))
      `(:pre ,(text s-exprs)))))

#+5am
(test parse-lisp
      "Test some <lisp>(code)</lisp>"
      (is (equalp (parse 'lisp "<lisp>(tapoueh-current-page-url)</lisp>")
		  '(:PRE "(tapoueh-current-page-url)"))))

;;; FIXME, actually parse and include the file with the right properties
;;; <include file="generate-pgloader-config.sql" markup="src" lang="sql">
(defrule include (and "<include" (? attrs) ">")
  (:lambda (source)
    (destructuring-bind (include attrs gt) source
      (declare (ignore include gt))
      `(:pre (:include ,@attrs)))))

(defun empty-string (string)
  (and (stringp string) (string= string "")))

(defrule paragraph (+ (or heavy bold italics monospace code link empty-line words))
  (:lambda (source)
    `(:p ,@(remove-if #'empty-string source))))

(defrule body (* (or centered
		     paragraph
		     title
		     class
		     src
		     literal
		     include
		     quote
		     lisp)))

(defrule article (and directives body)
  (:lambda (source)
    (destructuring-bind (document content) source
      (setf (muse-contents document) content)
      document)))

