+++
date = "2012-07-10T20:37:00.000000+02:00"
title = "Solving Every Sudoku Puzzle"
tags = ["Common-Lisp", "Sudoku", "Solver", "Puzzle"]
categories = ["Software Programming","Common Lisp"]
thumbnailImage = "/img/old/sudoku.png"
thumbnailImagePosition = "left"
coverImage = "/img/artificial-intelligence.640.jpg"
coverSize = "partial"
coverMeta = "in"
aliases = ["/blog/2012/07/10-solving-sudoku",
           "/blog/2012/07/10-solving-sudoku.html"]
+++

[Peter Norvig](http://norvig.com/) published a while ago a very nice article titled
[Solving Every Sudoku Puzzle](http://norvig.com/sudoku.html) wherein he presents a programmatic approach to
solving that puzzle game.

The article is very well written and makes it easy to think that coming up
with the code for such a solver is a very easy task, you apply some basic
problem search principles and there you are. Which is partly true, in fact.
Also, he uses 
`python`, and that means that a lot of trivial programming
activities are not a concern anymore, such as memory management.

{{< image classes="fig25 right dim-margin" src="/img/old/sudoku.320.png" >}}

As I've been teaching
myself [Common Lisp](http://www.cliki.net/Common%20Lisp) for some weeks now
I though I would like to read a lisp version of his code, and the article
even has a section titled *Translations*. Unfortunately, no lisp version is
available there. One might argue that [Clojure](http://clojure.org/) is a
decent enough lisp, but my current quest is all about *Common Lisp* really.
So I had to write one myself.

~~~ lisp
CL-USER> (sudoku:print-puzzle
	  (sudoku:solve-grid
"530070000600195000098000060800060003400803001700020006060000280000419005000080079"))
5 3 4 | 6 7 8 | 9 1 2 
6 7 2 | 1 9 5 | 3 4 8 
1 9 8 | 3 4 2 | 5 6 7 
------+-------+------
8 5 9 | 7 6 1 | 4 2 3 
4 2 6 | 8 5 3 | 7 9 1 
7 1 3 | 9 2 4 | 8 5 6 
------+-------+------
9 6 1 | 5 3 7 | 2 8 4 
2 8 7 | 4 1 9 | 6 3 5 
3 4 5 | 2 8 6 | 1 7 9 
took 1,974 microseconds (0.001974 seconds) to run.
During that period, and with 2 available CPU cores,
     1,894 microseconds (0.001894 seconds) were spent in user mode
        88 microseconds (0.000088 seconds) were spent in system mode
 174,320 bytes of memory allocated.
#<SUDOKU::PUZZLE #x3020023BB9FD>
~~~



## Comments on the python version

Norvig's article is very well written, I think. By that I mean that by
reading it you're confident that you've understood the problem and how the
solution is articulated, so you almost think you don't need to really try to
understand the code, it's just an illustration of the text.

Well, not so much. When you want to port the exact same algorithm you have
to understand exactly what the code is doing so that you're not implementing
something else. All the more when, as I did, you want to use some other data
structure.

My goal was not to rewrite the code as-is, but to try and come up with
*idiomatic* lisp code implementing Norvig's solution. So rather than using
*strings* and 
*dictionaries* (in lisp, they still call them a 
[hash table](http://www.lispworks.com/documentation/lw50/CLHS/Body/f_mk_has.htm)) I've
been using more natural data structures.

The 
*python* code is really not that easy to follow, full of functional
programming veteran tricks. I mean avoiding 
*exceptions* and simply
returning 
`False` whenever there's a problem, and using functions such as
`all` and 
`some` to manage that. It's certainly working, it's not making the
code any easier to read.

To summarize, that code looks like it's been written by someone smart who
didn't want to spend more than a couple of hours on it, and did take all
known trustworthy shortcuts he could to achieve that goal. Quality and
readability certainly weren't the key motive. I've been quite deceived after
reading a very good article.


## Comments on the common lisp version

Keep in mind that I'm just a 
*Common Lisp* newbie. I've been told some good
pieces of advice by knowledgeable people though, so with some luck my
implementation is somewhat 
*lispy* enough.

So we start by defining some data structures and low-level functions to
build up the more complex one, so that it's easier to read and debug. The
*sudoku* puzzle is then a grid of digits and a grid of possible values in
places where the digits are yet unknown.

The way to represent that 9x9 grid is with using 
[make-array](http://www.lispworks.com/documentation/lw51/CLHS/Body/f_mk_ar.htm):

~~~ lisp
(make-array '(9 9)
	    :element-type '(integer 0 9)
	    :initial-element 0)
~~~


Then the possible values. I though about using a 
`bit-vector` (and actually I
did implement it that way), then I've been told that the 
*Common Lisp* way to
approach that is using 
[2-complement integer representation](http://psg.com/~dlamkins/sl/chapter18.html), as we have
plenty of functions to operate numbers that way. I wouldn't believe that
would make the code simpler, but in fact it really did, see:

~~~ lisp
CL-USER> #b111111111
511
CL-USER> (logcount #b111111111)
9
CL-USER> (logcount 511)
9
CL-USER> (logbitp 3 #b100100100)
NIL
CL-USER> (logbitp 2 #b100100100)
T
CL-USER> (format nil "~2r" (logxor #b111111111 (ash 1 4)))
"111101111"
CL-USER> (logbitp 4 (logxor #b111111111 (ash 1 4)))
NIL
~~~


With that in mind, we can write the following code:

~~~ lisp
(defun count-remaining-possible-values (possible-values)
  "How many possible values are left in there?"
  ;; we could raise an empty-values condition if we get 0...
  (logcount possible-values))

(defun first-set-value (possible-values)
  "Return the index of the first set value in POSSIBLE-VALUES."
  (+ 1 (floor (log possible-values 2))))

(defun only-possible-value-is? (possible-values value)
  "Return a generalized boolean which is true when the only value found in
   POSSIBLE-VALUES is VALUE"
  (and (logbitp (- value 1) possible-values)
       (= 1 (logcount possible-values))))

(defun list-all-possible-values (possible-values)
  "Return a list of all possible values to explore"
  (loop for i from 1 to 9
     when (logbitp (- i 1) possible-values)
     collect i))

(defun value-is-set? (possible-values value)
  "Return a generalized boolean which is true when given VALUE is possible
   in POSSIBLE-VALUES"
  (logbitp (- value 1) possible-values))

(defun unset-possible-value (possible-values value)
  "return an integer representing POSSIBLE-VALUES with VALUE unset"
  (logxor possible-values (ash 1 (- value 1))))
~~~


You can see here that I was also under the influence of a recent reading
about
[making it obvious](http://gar1t.com/blog/2012/06/10/solving-embarrassingly-obvious-problems-in-erlang/),
or so
called
[intentional programming](http://dieswaytoofast.blogspot.fr/2012/07/erlang-why-so-many-seemingly-identical.html),
following what [Joe Armstrong](http://armstrongonsoftware.blogspot.fr/) has
to say about it:

>   Intentional programming is a name I give to a style of programming where
>   the reader of a program can easily see what the programmer intended by
>   their code. The intention of the code should be obvious from the names of
>   the functions involved and not be inferred by analysing the structure of
>   the code. (Reading the code should) precisely expresses the intention of
>   the programmer—here no guesswork or program analysis is involved, we
>   clearly read what was intended.


So there we go with function names such as
`count-remaining-possible-values`, that will help when reading some more
complex code, as in the following, the meat of the solution:

~~~ lisp
(defmethod eliminate ((puzzle puzzle) row col value)
  "Eliminate given VALUE from possible values in cell ROWxCOL of PUZZLE, and
   propagate when needed"
  (with-slots (grid values) puzzle
    ;; if already unset, work is already done
    (when (value-is-set? (aref values row col) value)
      ;; eliminate the value from the set of possible values
      (let* ((possible-values
	      (unset-possible-value (aref values row col) value)))
	(setf (aref values row col) possible-values)

	;; now if we're left with a single possible value
	(when (= 1 (count-remaining-possible-values possible-values))
	  (let ((found-value (first-set-value possible-values)))
	    ;; update the main grid
	    (setf (aref grid row col) found-value)

	    ;; eliminate that value we just found in all peers
	    (eliminate-value-in-peers puzzle row col found-value)))

	;; now check if any unit has a single possible place for that value
	(loop
	   for (r . c)
	   in (list-places-with-single-unit-solution puzzle row col value)
	   do (assign puzzle r c value))))))
~~~


So that lisp code is quite verbose and at 389 lines almost doubles the 201
lines Norvig had. When clarity is part of the goal, that's hard to avoid, I
hope I made a good case that this is not due to lisp being overly verbose by
itself.


## Comments on the development environment

Or why I even considered *Common Lisp* as an interesting language for that
kind of exercise, and some more. *I'll have to tell about re-sharding data
live with 16 threads and 256 databases, all in CL, someday*.

So I've been doing some *Emacs Lisp* development for a while now, and the
part that makes that so much fun is the instant reward. You write some code
in your editor, type a key chord (usually, that's `C-M-x runs the command
eval-defun`) and your code is loaded up, ready to be tested. In *Emacs Lisp*
the test can be simply using your editor and watching the new behavior
taking place, or playing in the `M-x ielm` console. When the code is not
ready, it crashes, and you're left in the interactive debugger, where you
can use `C-x C-e runs the command eval-last-sexp` to evaluate any expression
in your source and see its value in the current *debug frame*.

That way of working is a huge productivity boost, that I've been missing
much when getting back to writing C code for PostgreSQL. I can't `C-M-x` the
current function and go write some `SQL` to test it right away, I have to
*compile* the whole source tree, then *install* the new binaries, then
*restart* the test server and then open up a *psql* console to interact with
the new code. Of course I could just `make check` and watch the results, but
then if I attach a *debugger* it complains that the code on-disk is more
recent than the code in the *core dump*.

What if you want *Emacs Lisp* integrated facilities and something made for
general programming rather than suited to building a text editor? Don't get
me wrong, you can probably find more production ready code in *elisp* than
in many other languages, just because Emacs has been there for about 35
years. Editor targeted production code, though.

This integrated development cycle is all the same when you're using *Common
Lisp*. The
awesome
[Superior Lisp Interaction Mode for Emacs](http://common-lisp.net/project/slime/) is
providing exactly that experience. Just run `M-x slime` and then as you
define your code you can `C-M-x` the function at point, see the compilation
errors and warnings if any in the associated *REPL*, and just try your code.
I tend to mostly play in the command line, it's possible to just use `C-x
C-e` while typing too.


## Performances

Of course we do care! After all the original article came with a quite
detailed performance analysis with graphs and all. I won't be reproducing
that, sorry. I'll just show you what penalty you get for using an older
language specification, much more dynamic and with more features than
python, and with a great, scratch that, awesome development environment.

Oh wait, that's the other way round, no penalty, it's actually so much
faster!


### Python version perfs

The results I got on my desktop machine are about twice as fast as in the
original article, I guess newer machines and newer python have something to
say for that:

~~~ bash
dim ~/dev/CL/sudoku python sudoku.dim.py 
  All tests pass.
  Solved 50 of 50 easy puzzles (avg 0.01 secs (151 Hz), max 0.01 secs).
  Solved 95 of 95 hard puzzles (avg 0.02 secs (42 Hz), max 0.12 secs).
  Solved 11 of 11 hardest puzzles (avg 0.01 secs (115 Hz), max 0.01 secs).
~~~


That makes an average of `(50*151 + 95*42 + 11*115) / (50+95+11) = 82Hz`.

That seems pretty good, let's continue.

As you can see I've cut away the *random puzzle* part, that's because I was
too lazy to implement that part, which didn't seem all that interesting to
me. If you think that's a problem and need solving, I accept patches.


### Common lisp version perfs

When using [SBCL](http://sbcl.org/) on the same machine, what I got was:

~~~ lisp
(sudoku:solve-example-grids)
  Solved 50 of 50 easy puzzles (avg .0021 sec (471.7 Hz), max 0.015 secs).
  Solved 95 of 95 hard puzzles (avg .0022 sec (446.0 Hz), max 0.008 secs).
  Solved 11 of 11 hardest puzzles (avg .0018 sec (550.0 Hz), max 0.003 secs).
~~~


With the same way to compute the average, we now have `461.6Hz`.

Now, that's between 3 times and more than **10 times faster** than the
python version (taken collection per collection), for a comparable effort, a
much better development environment, and the same all dynamic no explicit
compiling approach.


## Conclusion

I guess I'm fond of *Common Lisp*, which I already saw coming (so did you,
right?), and now I have some public article and code to share about why :)

The code is hosted
at [https://github.com/dimitri/sudoku](https://github.com/dimitri/sudoku) if
you're interested, with the necessary files to reproduce, some docs, etc.

Also, apart from using *integers* as *bitfields*, which I did more for being
lispy than for performances, I did very little effort for optimizing the
code. It's quite naive in this respect, yet allow me an average of `461.6Hz`
rather than `82Hz`, that's ***5.6 times faster*** average.

So yes, I will continue to invest some precious time in *Common Lisp* as a
very good interactive scripting language, and maybe more than that.
