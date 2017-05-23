+++
date = "2012-08-22T16:05:00.000000+02:00"
title = "Fast and stupid?"
tags = ["Common-Lisp"]
categories = ["Software Programming","Common Lisp"]
thumbnailImage = "/img/old/made-with-lisp.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/made-with-lisp.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2012/08/20-performance-the-easiest-way",
           "/blog/2012/08/20-performance-the-easiest-way.html"]
+++

I stumbled onto an interesting article about performance when using python,
called 
[Python performance the easy(ish) way](http://jiaaro.com/python-performance-the-easyish-way), where the author tries to get
the bet available performances out of the dumbiest possible python code,
trying to solve a very simple and stupid problem.

With so many 
*smart* qualifiers you can only guess that I did love the
challenge. The idea is to write the simplest code possible and see how
smarter you need to be when you need perfs. Let's have a try!


## local python results

Here's the code I did use to benchmark the python solution:

~~~
def sumrange(arg):
    return sum(xrange(arg))

def sumrange2(arg):
    x = i = 0
    while i < arg:
        x += i
        i += 1
    return x


import ctypes
ct_sumrange = ctypes.CDLL('/Users/dim/dev/CL/jiaroo/sumrange.so')

def sumrange_ctypes(arg):
    return ct_sumrange.sumrange(arg)

if __name__ == "__main__":
    import timeit
    t1 = timeit.Timer('import jiaroo; jiaroo.sumrange(10**10)')
    t2 = timeit.Timer('import jiaroo; jiaroo.sumrange2(10**10)')
    ct = timeit.Timer('import jiaroo; jiaroo.sumrange_ctypes(10**10)')

    print 'timing python sumrange(10**10)'
    print 'xrange: %5fs' % t1.timeit(1)
    print 'while:  %5fs' % t2.timeit(1)
    print 'ctypes: %5fs' % ct.timeit(1)
~~~


Oh. And the C code too, sorry about that.

~~~
#include <stdio.h>

int sumrange(int arg)
{
    int i, x;
    x = 0;

    for (i = 0; i < arg; i++) {
        x = x + i;
    }
    return x;
}
~~~


And here's how I did compile it. The author of the inspiring article
insisted on stupid optimisation targets, I did follow him:

~~~
gcc -shared -Wl,-install_name,sumrange.so -o sumrange.so -fPIC sumrange.c -O0
~~~


And here's the result I did get out of it:

~~~
python jiaroo.py
timing python sumrange(10**10)
xrange: 927.039917s
while:  2377.291237s
ctypes: 5.297124s
~~~


Let's be fair, with 
`-O2` we get much better results:

~~~
timing python sumrange(10**10)
ctypes: 1.065684s
~~~



## Common Lisp to the rescue

So let's have a try in Common Lisp, will you ask me, right?

Here's the code I did use, you can see three different tries:

~~~
;;;; jiaroo.lisp
;;;
;;; See http://jiaaro.com/python-performance-the-easyish-way
;;;
;;; The goal here is to find out if CL needs to resort to C for very simple
;;; optimisation tricks like python apparently needs too, unless using pypy
;;; (to some extend).

(in-package #:jiaroo)

;;; "jiaroo" goes here. Hacks and glory await!

(defun sumrange-loop (max)
  "return the sum of numbers from 1 to MAX"
  (let ((sum 0))
    (declare (type (and unsigned-byte fixnum) max sum)
	     (optimize speed))
    (loop for i fixnum from 1 to max do (incf sum i))))

(defun sumrange-dotimes (max)
  "return the sum of numbers from 1 to MAX"
  (let ((sum 0))
    (declare (type (and unsigned-byte fixnum) max sum)
	     (optimize speed))
    (dotimes (i max sum)
      (incf sum i))))

(defun pk-sumrange (max)
  (declare (type (and unsigned-byte fixnum) max)
	   (optimize speed))
  (let ((sum 0))
    (declare (type (and fixnum unsigned-byte) sum))
    (dotimes (i max sum)
      (setf sum (logand (+ sum i) most-positive-fixnum)))))

(defmacro timing (&body forms)
  "return both how much real time was spend in body and its result"
  (let ((start (gensym))
	(end (gensym))
	(result (gensym)))
    `(let* ((,start (get-internal-real-time))
	    (,result (progn ,@forms))
	    (,end (get-internal-real-time)))
       (values ,result (/ (- ,end ,start) internal-time-units-per-second)))))

(defun bench-sumrange (power)
  "print execution time of both the previous functions"
  (let* ((max (expt 10 power))
	 (lp-time (multiple-value-bind (r s) (timing (sumrange-loop max)) s))
	 (dt-time (multiple-value-bind (r s) (timing (sumrange-dotimes max)) s))
	 (pk-time (multiple-value-bind (r s) (timing (pk-sumrange max)) s)))
    (format t "timing common lisp sumrange 10**~d~%" power)
    (format t "loop:       ~2,3fs ~%" lp-time)
    (format t "dotimes:    ~2,3fs ~%" dt-time)
    (format t "pk dotimes: ~2,3fs ~%" pk-time)))
~~~


And here's the results:

~~~
CL-USER> (bench-sumrange 10)
timing common lisp sumrange 10**10
loop:       11.213s 
dotimes:    7.642s 
pk dotimes: 22.185s 
NIL
~~~



## Discussion

So python is very slow. C is pretty fast. And Common Lisp just in the
middle. Honnestly I expected better performances from my beloved Common Lisp
here, but I didn't try very hard, by using 
[Clozure Common Lisp](http://ccl.clozure.com/) which is not
the quicker Common Lisp implementation around. For this very benchmark, if
you're seeking speed use either 
[Steel Bank Common Lisp](http://sbcl.org/) or 
[CLISP](http://www.clisp.org/) which is
known to have a pretty fast bignums implementation (which you don't need in
64 bits in that game).

On the other hand, I think that having to go write a C plugin and deal with
how to compile and deploy it in the middle of a python script is something
to avoid. When using Common Lisp you don't need to resort to that for the
*runtime* to get down from python 
*xrange* implementation at 
`927.039917s` down to
the 
*dotimes* implementation taking 
`7.642s`. That's about 
`121` times faster.

So while 
`C` is even better, and while I would like a Common Lisp guru to show
me how to get a better speed here, I still very much appreciate the solution
here.

Let's see the winning source code in 
*python* and 
*common lisp* to compare the
programmer side of things: how hard was it really to get 
`121` times faster?

~~~
def sumrange(arg):
    return sum(xrange(arg))
~~~


~~~
(defun sumrange-dotimes (max)
  "return the sum of numbers from 1 to MAX"
  (let ((sum 0))
    (declare (type (and unsigned-byte fixnum) max sum)
	     (optimize speed))
    (dotimes (i max sum)
      (incf sum i))))
~~~


That's about it. Yes we can see some 
*manual* optimisation directives here,
which are optimisation 
*extra complexity*. Not to the same level as bringing a
compiled artifact that you need to build and deploy, though. Remember that
you will need to know the full path where to find the 
`sumrange.so` file on
the production system, in the optimised 
*python* case, so that's what we are
comparing against.

Here's what happens without the optimisation, and with a smaller target:

~~~
CL-USER> (time (jiaroo:sumrange-dotimes (expt 10 9)))
(JIAROO:SUMRANGE-DOTIMES (EXPT 10 9))
took 722,592 microseconds (0.722592 seconds) to run.
During that period, and with 2 available CPU cores,
     714,709 microseconds (0.714709 seconds) were spent in user mode
       1,183 microseconds (0.001183 seconds) were spent in system mode
499999999500000000

CL-USER> (time (let ((sum 0)) (dotimes (i (expt 10 9) sum) (incf sum i))))
(LET ((SUM 0)) (DOTIMES (I (EXPT 10 9) SUM) (INCF SUM I)))
took 2,174,767 microseconds (2.174767 seconds) to run.
During that period, and with 2 available CPU cores,
     2,156,549 microseconds (2.156549 seconds) were spent in user mode
        10,225 microseconds (0.010225 seconds) were spent in system mode
499999999500000000
~~~


We get a 
`3` times speed-up from those 2 lines of lisp optimisation
directives, which is pretty good. And it's exponential as I didn't have the
patience to actually wait until the non optimised 
`10^10` run finished, I
killed it.


## Conclusion

That's a case here where I don't know how to reach 
`C` level of performances
with Common Lisp, which could just be because I don't know yet how to do.

Still, getting a 
`121` times speedup when compared to the pure 
*python* version
of the code is pretty good and encourages me to continue diving into Common
Lisp.
