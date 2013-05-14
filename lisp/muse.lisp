;;;; muse.lisp
;;;
;;; Parse the Emacs Muse format from the Muse project, as found at:
;;;
;;;   http://mwolson.org/projects/EmacsMuse.html

(in-package #:tapoueh)

(def-suite muse :description "Emacs Muse Parser Test Suite.")
(in-suite muse)

;;
;; Document data structure, pretty loose
;;
(defstruct
    (muse
      (:constructor make-muse (&key
			       author title date tags contents
			       &aux (timestamp (parse-date date)))))
  author title date timestamp tags contents)

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
	   ,(muse-contents document))))

;;
;; Basics, generics
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

(defrule directive-name (or directive-author
			    directive-title
			    directive-date
			    directive-tags))

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
      (apply #'make-muse (apply #'append directives)))))

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
	   (make-muse :author "Dimitri Fontaine"
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

(defrule word (+ (muse-word-p character))
  (:text t))

(defrule words (+ (or word whitespace))
  (:lambda (source)
    (apply #'concatenate 'string source)))

(defrule namestring (and (alpha-char-p character)
			 (* (or (alpha-char-p character)
				(digit-char-p character))))
  (:text t))

(defrule hostname (and namestring (? (and "." hostname)))
  (:text t))

(defun http-uri-chararcter-p (char)
  "Allowed characters in an URI, including parameters"
  (or (member char #.(quote (coerce "`~!@#$%^&*()-_+{}\\|;:'\"<>,./?=" 'list)))
      (alphanumericp char)))

(defrule http-localpath-and-args (* (http-uri-chararcter-p character))
  (:text t))

(defrule http-protocol (and "http" (? "s") "://")
  (:text t))

(defrule http-uri (and http-protocol hostname "/" http-localpath-and-args)
  (:lambda (source)
    (destructuring-bind (proto hostname slash localpath) source
      (concatenate 'string proto hostname slash localpath))))

(defun filename-character-p (char)
  (or (member char #.(quote (coerce "/:.-_@$%^*" 'list)))
      (alphanumericp char)))

(defrule filename (* (filename-character-p character))
  (:text t))

(defrule link-label (and "[" words "]")
  (:lambda (source)
    (destructuring-bind (open label close) source
      (declare (ignore open close))
      (text label))))

(defrule link-target (and "[" (or http-uri filename) "]")
  (:lambda (source)
    (destructuring-bind (open target close) source
      (declare (ignore open close))
      target)))

(defrule link (and "[" link-target (? link-label) "]")
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
	   :test #'equalp)))

(defrule monospace (and #\= words #\=)
  (:lambda (source)
    (destructuring-bind (open content close) source
      (declare (ignore open close))
      `(:span :class "tt" ,content))))

(defrule italics (and #\* words #\*)
  (:lambda (source)
    (destructuring-bind (open content close) source
      (declare (ignore open close))
      `(:em ,content))))

(defrule bold (and #\* #\* words #\* #\*)
  (:lambda (source)
    (destructuring-bind (open1 open2 content close1 close2) source
      (declare (ignore open1 open2 close1 close2))
      `(:strong ,content))))

(defrule heavy (and #\* #\* #\* words #\* #\* #\*)
  (:lambda (source)
    (destructuring-bind (o1 o2 o3 content c1 c2 c3) source
      (declare (ignore o1 o2 o3 c1 c2 c3))
      `(:em (:strong ,content)))))

#+5am
(test parse-emphasis
      "Test *italics* and **bold** and ***heavy*** etc."
      (is (equalp (parse 'monospace "=code=")
		  '(:span :class "tt" "code")))
      (is (equalp (parse 'italics "*some italic words*")
		  '(:em  "some italic words")))
      (is (equalp (parse 'bold "**this is bold**")
		  '(:strong "this is bold")))
      (is (equalp (parse 'heavy "***this is heavy***")
		  '(:em (:strong "this is heavy")))))

(defrule centered (and #\Tab
		       (or heavy bold italics monospace link words)
		       (? #\Newline))
  (:lambda (source)
    (destructuring-bind (tab thing nl) source
      (declare (ignore tab nl))
      `(:center ,thing))))

#+5am
(test parse-centered
      "Test some centered (tabulated) contents"
      (is (equalp (parse 'centered "	*ahah*")
		  '(:center (:em  "ahah"))))
      (is (equalp (parse 'centered "	*ahah*
")
		  '(:center (:em  "ahah")))))

(defun empty-string (string)
  (and (stringp string) (string= string "")))

(defrule paragraph (+ (or heavy bold italics monospace link empty-line words))
  (:lambda (source)
    `(:p ,@(remove-if #'empty-string source))))

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

(defrule src-attrs-lang
    (and whitespaces "lang=\"" (+ (or #\- (alpha-char-p character))) "\"")
  (:lambda (source)
    (destructuring-bind (ws lang value quote) source
      (declare (ignore ws lang quote))
      (list :lang (text value)))))

(defrule src (and "<src" (? src-attrs-lang) ">"
		  (+ (not "</src>"))
		  "</src>")
  (:lambda (source)
    (destructuring-bind (open attrs gt source close) source
      (declare (ignore open attrs gt close))
      `(:pre ,(text source)))))

#+5am
(test parse-src
      "Test some <src>content</src>"
      (is (equalp (parse 'src "<src>your code snippet here</src>")
		  '(:PRE "your code snippet here"))))

(defrule body (* (or centered paragraph title src)))

(defrule article (and directives body)
  (:lambda (source)
    (destructuring-bind (document content) source
      (setf (muse-contents document) content)
      document)))

