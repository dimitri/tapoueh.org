+++
date = "2013-05-13T11:08:00.000000+02:00"
title = "from Parsing to Compiling"
tags = ["Common-Lisp"]
categories = ["Software Programming","Common Lisp"]
thumbnailImage = "/img/old/lightbulb.gif"
thumbnailImagePosition = "left"
coverImage = "/img/Light_bulb_idea.jpg"
coverSize = "partial"
coverMeta = "in"
aliases = ["/blog/2013/05/13-from-parser-to-compiler",
           "/blog/2013/05/13-from-parser-to-compiler.html"]
+++

Last week came with two bank holidays in a row, and I took the opportunity
to design a *command language* for [pgloader](../../../pgsql/pgloader.html).
While doing that, I unexpectedly stumbled accross a very nice *AHAH!*
moment, and I now want to share it with you, dear reader.

<!--more-->
<!--toc-->

The general approach I'm following code wise with that *command language* is
to first get a code API to expose the capabilities of the system, then
somehow plug the *command language* into that API thanks to a *parser*. It
turns out that doing so in *Common Lisp* is really easy, and that you can
get a *compiler* for free too, while at it. Let's see about that.


# A very simple toy example

In this newsgroup
article
[What is symbolic computation?](https://groups.google.com/forum/?fromgroups=#!topic/comp.lang.lisp/JJxTBqf7scU),
[Pascal Bourguignon](http://informatimago.com/) did propose a very simple
piece of code:

~~~ lisp
(defparameter *additive-color-graph*
  '((red   (red white)   (green yellow) (blue magenta))
    (green (red yellow)  (green white)  (blue cyan))
    (blue  (red magenta) (green cyan)   (blue white))))

(defun symbolic-color-add (a b)
  (cadr (assoc a (cdr (assoc b *additive-color-graph*)))))
~~~


This is an example of *symbolic computation*, and we're going to build a
little *language* to express the data and the code. Not that we would need
to build one, mind you, more in order to have a really simple example
leading us to the *ahah* moment you're now waiting for.

Before we dive into the main topic, you have to realize that the previous
code example actually works: it's defining some data, using an implicit data
structure composed by nesting lists together, and defines a function that
knows how to sort out the data in that anonymous data structure so as to
compound 2 colors together.

~~~ lisp
TOY-PARSER> (symbolic-color-add 'red 'green)
YELLOW
~~~



# A command language and parser

I decided to go with the following *language*:

~~~
color red   +red white    +green yellow  +blue magenta
color green +red yellow   +green white   +blue cyan
color blue  +red magenta  +green cyan    +blue white

mix red and green
~~~


And here's how some of the parser looks like, using
the [esrap](http://nikodemus.github.io/esrap/) *packrat* lib:

~~~ lisp
(defrule color-name (and whitespaces (+ (alpha-char-p character)))
  (:destructure (ws name)
    (declare (ignore ws))		; ignore whitespaces
    ;; CL symbols default to upper case.
    (intern (string-upcase (coerce name 'string)) :toy-parser)))

;;; parse string "+ red white"
(defrule color-mix (and whitespaces "+" color-name color-name)
  (:destructure (ws plus color-added color-obtained)
    (declare (ignore ws plus))		; ignore whitespaces and keywords
    (list color-added color-obtained)))

;;; mix red and green
(defrule mix-two-colors (and kw-mix color-name kw-and color-name)
  (:destructure (mix c1 and c2)
    (declare (ignore mix and))		; ignore keywords
    (list c1 c2)))
~~~

{{< image classes="fig50 right dim-margin"
              src="/img/old/the-one-ring.320.jpg"
            title="The one grammar rule to bind them all">}}

Those *rules* are not the whole parser, go have a look at the project on
github if you want to see the whole code, it's
called [toy-parser](https://github.com/dimitri/toy-parser) over there. The
main idea here is to show that when we parse a line from our little
language, we produce the simplest possible structured data: in lisp that's
*symbols* and *lists*.

The reason why it makes sense doing that is the next rule:

~~~ lisp
(defrule program (and colors mix-two-colors)
  (:destructure (graph (c1 c2))
    `(lambda ()
       (let ((*additive-color-graph* ',graph))
	 (symbolic-color-add ',c1 ',c2)))))
~~~


This rule is the complex one to bind them all. It's using a *quasiquote*, a
basic lisp syntax element allowing the programmer to very easily produce
data that looks exactly like code. Let's see how it goes with a very simple
example:

~~~ lisp
TOY-PARSER> (pprint (parse 'program
                           "color red +green yellow mix green and red"))

(LAMBDA NIL
  (LET ((*ADDITIVE-COLOR-GRAPH* '((RED (GREEN YELLOW)))))
    (SYMBOLIC-COLOR-ADD 'RED 'GREEN)))
; No value
~~~


The parser is producing structure (nested) data that really looks like lisp
code, right? So maybe we can just run that code...


# What about a compiler now?

Let's see about actually running the code:

~~~ lisp
TOY-PARSER> (let* ((code "color red +green yellow mix green and red")
		   (program (parse 'program code)))
	      (compile nil program))
#<Anonymous Function #x3020027CF0EF>
NIL
NIL
TOY-PARSER> (let* ((code "color red +green yellow mix green and red")
		   (program (parse 'program code)))
	      (funcall (compile nil program)))
YELLOW
~~~


So we have a string reprensing code in our very little language, and a
parser that knows how to produce a nested list of atoms that looks like lisp
code. And as we have lisp, we can actually compile that code at run-time
with the same compiler that we used to produce our parser, and we can then
`funcall` that function we just built.

Oh and the function is actually compiled down to native code, of course:

~~~ lisp
TOY-PARSER> (let* ((code "color red +green yellow mix red and green")
		   (program (parse 'program code))
		   (func    (compile nil program)))
	      (time (loop repeat 1000 do (funcall func))))

(LOOP REPEAT 1000 DO (FUNCALL FUNC))
took 108 microseconds (0.000108 seconds) to run.
During that period, and with 4 available CPU cores,
     105 microseconds (0.000105 seconds) were spent in user mode
      13 microseconds (0.000013 seconds) were spent in system mode
NIL
~~~


Yeah, it took the whole of 
`108 microseconds` to actually run the code
generated by our own 
*parser* 
**a thousand times**, on my laptop. I can believe
it's been compiled to native code, that seems like the right ballpark.


# Conclusion

The 
[toy-parser](https://github.com/dimitri/toy-parser) code is there on 
*GitHub* and you can actually load it using
[Quicklisp](http://www.quicklisp.org/): clone the repository in 
`~/quicklisp/local-projects/` then
`(ql:quickload "toy-parser")`, and play with it in 
`(in-package :toy-parser)`.

The only thing I still want to say here is this: can your programming
language of choice make it that easy?
