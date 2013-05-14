((:P
               "Last week came with two bank holidays in a row, and I took the opportunity
to design a "
               (:EM "command language") " for "
               (:A :HREF "../../../pgsql/pgloader.html" "pgloader")
               ". While doing that, I unexpectedly
stumbled accross a very nice "
               (:EM "AHAH!") " moment, and I now want to share it with
you, dear reader.

")
              (:CENTER (:IMG :SRC "../../../images/lightbulb.gif")) (:P)
              (:CENTER (:EM "AHAH, you'll see!"))
              (:P "The general approach I'm following code wise with that "
               (:EM "command language") " is
to first get a code API to expose the capabilities of the system, then
somehow plug the "
               (:EM "command language") " into that API thanks to a "
               (:EM "parser") ". It turns
out that doing so in "
               (:EM "Common Lisp") " is really easy, and that you can get a
"
               (:EM "compiler")
               " for free too, while at it. Let's see about that.

")
              (:H2 "A very simple toy example")
              (:P "In this newsgroup article "
               (:A :HREF
                "https://groups.google.com/forum/?fromgroups=#!topic/comp.lang.lisp/JJxTBqf7scU"
                "What is symbolic compoutation?")
               ", " (:A :HREF "http://informatimago.com/" "Pascal Bourguignon")
               "did propose a very simple piece of code:

")
              (:PRE "
(defparameter *additive-color-graph*
  '((red   (red white)   (green yellow) (blue magenta))
    (green (red yellow)  (green white)  (blue cyan))
    (blue  (red magenta) (green cyan)   (blue white))))

(defun symbolic-color-add (a b)
  (cadr (assoc a (cdr (assoc b *additive-color-graph*)))))
")
              (:P "This is an example of " (:EM "symbolic computation")
               ", and we're going to build a
little "
               (:EM "language")
               " to express the data and the code. Not that we would need to
build one, mind you, more in order to have a really simple example leading
us to the "
               (:EM "ahah") " moment you're now waiting for.

Before we dive into the main topic, you have to realize that the previous
code example actually works: it's defining some data, using an implicit data
structure composed by nesting lists together, and defines a function that
knows how to sort out the data in that anonymous data structure so as to
compound 2 colors together.

")
              (:PRE "
TOY-PARSER> (symbolic-color-add 'red 'green)
YELLOW
")
              (:P) (:H2 "A command language and parser")
              (:P "I decided to go with the following " (:EM "language") ":

")
              (:PRE "
color red   +red white    +green yellow  +blue magenta
color green +red yellow   +green white   +blue cyan
color blue  +red magenta  +green cyan    +blue white

mix red and green
")
              (:P "And here's how some of the parser looks like, using the "
               (:A :HREF "http://nikodemus.github.io/esrap/" "esrap") " "
               (:EM "packrat") " lib:

")
              (:PRE "
(defrule color-name (and whitespaces (+ (alpha-char-p character)))
  (:destructure (ws name)
    (declare (ignore ws))		; ignore whitespaces
    ;; CL symbols default to upper case.
    (intern (string-upcase (coerce name 'string)) :toy-parser)))

;;; parse string \"+ red white\"
(defrule color-mix (and whitespaces \"+\" color-name color-name)
  (:destructure (ws plus color-added color-obtained)
    (declare (ignore ws plus))		; ignore whitespaces and keywords
    (list color-added color-obtained)))

;;; mix red and green
(defrule mix-two-colors (and kw-mix color-name kw-and color-name)
  (:destructure (mix c1 and c2)
    (declare (ignore mix and))		; ignore keywords
    (list c1 c2)))
")
              (:P "Those " (:EM "rules")
               " are not the whole parser, go have a look at the project on
github if you want to see the whole code, it's called "
               (:A :HREF "https://github.com/dimitri/toy-parser" "toy-parser")
               " over there.
The main idea here is to show that when we parse a line from our little
language, we produce the simplest possible structured data: in lisp that's
"
               (:EM "symbols") " and " (:EM "lists") ".

The reason why it makes sense doing that is the next rule:

")
              (:CENTER (:IMG :SRC "../../../images/the-one-ring.jpg")) (:P)
              (:CENTER (:EM "The one grammar rule to bind them all")) (:P)
              (:PRE "
(defrule program (and colors mix-two-colors)
  (:destructure (graph (c1 c2))
    `(lambda ()
       (let ((*additive-color-graph* ',graph))
	 (symbolic-color-add ',c1 ',c2)))))
")
              (:P
               "This rule is the complex one to bind them all. It's using a "
               (:EM "quasiquote") ", a
basic lisp syntax element allowing the programmer to very easily produce
data that looks exactly like code. Let's see how it goes with a very simple
example:

")
              (:PRE "
TOY-PARSER> (pprint (parse 'program
                           \"color red +green yellow mix green and red\"))

(LAMBDA NIL
  (LET ((*ADDITIVE-COLOR-GRAPH* '((RED (GREEN YELLOW)))))
    (SYMBOLIC-COLOR-ADD 'RED 'GREEN)))
; No value
")
              (:P
               "The parser is producing structure (nested) data that really looks like lisp
code, right? So maybe we can just run that code...

")
              (:H2 "What about a compiler now?") (:P)
              (:CENTER (:IMG :SRC "../../../images/aha.jpg")) (:P)
              (:CENTER (:EM "Here is my AHAH moment!"))
              (:P "Let's see about actually running the code:

")
              (:PRE "
TOY-PARSER> (let* ((code \"color red +green yellow mix green and red\")
		   (program (parse 'program code)))
	      (compile nil program))
#<Anonymous Function #x3020027CF0EF>
NIL
NIL
TOY-PARSER> (let* ((code \"color red +green yellow mix green and red\")
		   (program (parse 'program code)))
	      (funcall (compile nil program)))
YELLOW
")
              (:P
               "So we have a string reprensing code in our very little language, and a
parser that knows how to produce a nested list of atoms that looks like lisp
code. And as we have lisp, we can actually compile that code at run-time
with the same compiler that we used to produce our parser, and we can then
"
               (:SPAN :CLASS "tt" "funcall") " that function we just built.

Oh and the function is actually compiled down to native code, of course:

")
              (:PRE "
TOY-PARSER> (let* ((code \"color red +green yellow mix red and green\")
		   (program (parse 'program code))
		   (func    (compile nil program)))
	      (time (loop repeat 1000 do (funcall func))))

(LOOP REPEAT 1000 DO (FUNCALL FUNC))
took 108 microseconds (0.000108 seconds) to run.
During that period, and with 4 available CPU cores,
     105 microseconds (0.000105 seconds) were spent in user mode
      13 microseconds (0.000013 seconds) were spent in system mode
NIL
")
              (:P "Yeah, it took the whole of "
               (:SPAN :CLASS "tt" "108 microseconds") " to actually run the code
generated by our own "
               (:EM "parser") " " (:STRONG "a thousand times")
               ", on my laptop. I can believe
it's been compiled to native code, that seems like the right ballpark.

")
              (:H2 "Conclusion")
              (:P "The "
               (:A :HREF "https://github.com/dimitri/toy-parser" "toy-parser")
               " code is there on " (:EM "GitHub")
               " and you can actually load it using
"
               (:A :HREF "http://www.quicklisp.org/" "Quicklisp")
               ": clone the repository in "
               (:SPAN :CLASS "tt" "~/quicklisp/local-projects/") " then
"
               (:SPAN :CLASS "tt" "(ql:quickload \"toy-parser\")")
               ", and play with it in "
               (:SPAN :CLASS "tt" "(in-package :toy-parser)") ".

The only thing I still want to say here is this: can your programming
language of choice make it that easy?
"))
