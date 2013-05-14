;;;; muse.lisp
;;;
;;; Parse the Emacs Muse format from the Muse project, as found at:
;;;
;;;   http://mwolson.org/projects/EmacsMuse.html

(in-package #:tapoueh)

;;
;; Document data structure, pretty loose
;;
(defstruct
    (muse
      (:constructor make-muse (&key
			       author title date tags contents
			       &aux (timestamp (parse-date date)))))
  author title date timestamp tags contents)

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

(defrule centered (and #\Tab
		       (or heavy bold italics monospace link words)
		       (? #\Newline))
  (:lambda (source)
    (destructuring-bind (tab thing nl) source
      (declare (ignore tab nl))
      `(:center ,thing))))

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

(defrule body (* (or centered paragraph title src)))

(defrule article (and directives body)
  (:lambda (source)
    (destructuring-bind (document content) source
      (setf (muse-contents document) content)
      document)))

(defun test (&key
	       (expression 'article)
	       (filename "/tmp/13-from-parser-to-compiler.muse")
	       junk-allowed)
  (let ((content (slurp-file-into-string filename)))
    (parse expression content :junk-allowed junk-allowed)))

(defmethod to-html ((document muse))
  "Produce the HTML of given Document"
  (eval `(with-html-output-to-string (s nil :indent t)
	   ,(muse-contents document))))
