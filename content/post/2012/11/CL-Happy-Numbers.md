+++
date = "2012-11-20T18:20:00.000000+01:00"
title = "CL Happy Numbers"
tags = ["Common-Lisp"]
categories = ["Software Programming","Common Lisp"]
thumbnailImage = "/img/old/happy-numbers.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/happy-numbers.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2012/11/20-CL-Happy-Numbers",
           "/blog/2012/11/20-CL-Happy-Numbers.html"]
+++

A while ago I stumbled
upon [Happy Numbers](http://tapoueh.org/blog/2010/08/30-happy-numbers.html)
as explained
in
[programming praxis](http://programmingpraxis.com/2010/07/23/happy-numbers/),
and offered an implementation of them in `SQL` and in `Emacs Lisp`. Yeah, I
know. Why not, though?

Today I'm back on that topic and as I'm toying with *Common Lisp* I though
it would be a good excuse to learn me some new tricks. As you can see from
the earlier blog entry, last time I did attack the *digits* problem quite
lightly. Let's try a better approach now.

~~~ lisp
(defun digits (n)
  "return the list of the digits of N"
  (nreverse
   (loop :for x := n :then r
      :for (r d) := (multiple-value-list (truncate x 10))
      :collect d
      :until (zerop r))))
~~~


As you can see I wanted to use that facility I like very much, the `for x =
n then r` way to handle first loop iteration differently from the next ones.
But I've been hinted on `#lisp` that there's a much better way to write same
code:

~~~ lisp
(defun integer-digits (integer)
  "stassats version"
  (nreverse
   (loop :with remainder
      :do (setf (values integer remainder) (truncate integer 10))
      :collect remainder
      :until (zerop integer))))
~~~


That code runs about twice as fast as the previous one and is easier to
reason about. It's using `setf` and the
form
[setf values](http://www.lispworks.com/documentation/lw51/CLHS/Body/f_values.htm),
something nice to discover as it seems to be quite powerful. Let's see how
to use it, even if it's really simple:

~~~ lisp
CL-USER> (integer-digits 12304501)
(1 2 3 0 4 5 0 1)
~~~


Let's move on to solving the *Happy Numbers* problem though:

~~~ lisp
(defun sum-of-squares-of-digits (integer)
  (loop :with remainder
     :do (setf (values integer remainder) (truncate integer 10))
     :sum (* remainder remainder)
     :until (zerop integer)))

(defun happy? (n &optional seen)
  "return true when n is a happy number"
  (let* ((happiness (sum-of-squares-of-digits n)))
    (cond ((eq 1 happiness)      t)
	  ((memq happiness seen) nil)
	  (t
	   (happy? happiness (push happiness seen))))))

(defun find-happy-numbers (limit)
  "find all happy numbers from 1 to limit"
  (loop :for n :from 1 :to limit :when (happy? n) :collect n))
~~~


And here's how it goes:

~~~ lisp
CL-USER> (find-happy-numbers 100)
(1 7 10 13 19 23 28 31 32 44 49 68 70 79 82 86 91 94 97 100)

CL-USER> (time (length (find-happy-numbers 1000000)))
(LENGTH (FIND-HAPPY-NUMBERS 1000000))
took 1,621,413 microseconds (1.621413 seconds) to run.
       116,474 microseconds (0.116474 seconds, 7.18%) of which was spent in GC.
During that period, and with 4 available CPU cores,
     1,431,332 microseconds (1.431332 seconds) were spent in user mode
       145,941 microseconds (0.145941 seconds) were spent in system mode
 185,438,208 bytes of memory allocated.
 1 minor page faults, 0 major page faults, 0 swaps.
143071
~~~


Of course that code is much faster than the one I wrote before both in `SQL`
and *Emacs Lisp*, the reason being that instead of writing the number into a
*string* with `(format t "~d" number)`
then
[subseq](http://www.lispworks.com/documentation/HyperSpec/Body/f_subseq.htm)
to get them one after the other, we're now
using
[truncate](http://www.lispworks.com/documentation/HyperSpec/Body/f_floorc.htm).

Happy hacking!


## Update

It turns out that to solve math related problem, some maths hindsight is
helping. Who would have believed that? So if you want to easily get some
more performances out of the previous code, just try that solution:

~~~ lisp
(defvar *depressed-squares* '(0 4 16 20 37 42 58 89 145)
  "see http://oeis.org/A039943")

(defun undepressed? (n)
  "same as happy?, using a static list of unhappy sums"
  (cond ((eq 1 n) t)
	((member n *depressed-squares*) nil)
	(t
	 (let ((h (sum-of-squares-of-digits n)))
	   (undepressed? h)))))

(defun find-undepressed-numbers (limit)
  "find all happy numbers from 1 to limit"
  (loop :for n :from 1 :to limit :when (undepressed? n) :collect n))
~~~


Time to compare:

~~~ lisp
CL-USER> (time (length (find-happy-numbers 1000000)))
(LENGTH (FIND-HAPPY-NUMBERS 1000000))
took 1,938,048 microseconds (1.938048 seconds) to run.
       290,902 microseconds (0.290902 seconds, 15.01%) of which was spent in GC.
During that period, and with 4 available CPU cores,
     1,778,021 microseconds (1.778021 seconds) were spent in user mode
       140,862 microseconds (0.140862 seconds) were spent in system mode
 185,438,208 bytes of memory allocated.
 3,320 minor page faults, 0 major page faults, 0 swaps.
143071

CL-USER> (time (length (find-undepressed-numbers 1000000)))
(LENGTH (FIND-UNDEPRESSED-NUMBERS 1000000))
took 1,036,847 microseconds (1.036847 seconds) to run.
         5,372 microseconds (0.005372 seconds, 0.52%) of which was spent in GC.
During that period, and with 4 available CPU cores,
     1,018,708 microseconds (1.018708 seconds) were spent in user mode
        16,982 microseconds (0.016982 seconds) were spent in system mode
 2,289,152 bytes of memory allocated.
143071
CL-USER> 
~~~

