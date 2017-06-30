+++
date = "2012-11-04T23:04:00.000000+01:00"
title = "Concurrent Hello"
tags = ["Common-Lisp"]
categories = ["Software Programming","Common Lisp"]
thumbnailImage = "/img/prog-lisp_icon-icons.png"
thumbnailImagePosition = "left"
coverImage = "/img/multithreaded-programming.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2012/11/04-Concurrent-Hello",
           "/blog/2012/11/04-Concurrent-Hello.html"]
+++

Thanks to [Mickael](https://twitter.com/mickael/status/265191809100181504)
on *twitter* I ran into that article about implementing a very basic *Hello
World!* program as a way to get into a new concurrent language or facility.
The original article,
titled
[Concurrent Hello World in Go, Erlang and C++](http://himmele.blogspot.de/2012/11/concurrent-hello-world-in-go-erlang.html) is
all about getting to know [The Go Programming Language](http://golang.org/)
better.

To quote the article:

> The first thing I always do when playing around with a new
> software platform is to write a concurrent "Hello World" program. The
> program works as follows: One active entity (e.g. thread, Erlang process,
> Goroutine) has to print "Hello " and another one "World!\n" with the two
> active entities synchronizing with each other so that the output always is
> "Hello World!\n".


Here's my try in [Common Lisp](http://cliki.net/)
using [lparallel](http://lparallel.org/) and some *local nicknames*, the
whole `23` lines of it:

~~~ lisp
(defun say-hello (helloq worldq n)
  (dotimes (i n)
    (format t "Hello ")
    (lq:push-queue :say-world worldq)
    (lq:pop-queue helloq))
  (lq:push-queue :quit worldq))

(defun say-world (helloq worldq)
  (when (eq (lq:pop-queue worldq) :say-world)
    (format t "World!~%")
    (lq:push-queue :say-hello helloq)
    (say-world helloq worldq)))

(defun hello-world (n)
  (let* ((lp:*kernel*  (lp:make-kernel 2)) ; a new one each time, as we end it
	 (channel      (lp:make-channel))
	 (helloq       (lq:make-queue))
	 (worldq       (lq:make-queue)))
    (lp:submit-task channel #'say-world helloq worldq)
    (lp:submit-task channel #'say-hello helloq worldq n)
    (lp:receive-result channel)
    (lp:receive-result channel)
    (lp:end-kernel)))
~~~


If you want to play locally with that code, I've been updating it to a
*github* project
named [go-hello-world](https://github.com/dimitri/go-hello-world), even if
it's coded in *CL*. See the `package.lisp` in there for how I did enable the
*local nicknames* `lp` and `lq` for the *lparallel* packages.


## Beware of the REPL

In a previous version of this very article, I said that sometimes I get an
extra line feed in the output and I didn't understand why. Some great Common
Lisp folks did hint me about that: it's the *REPL* output that get
intermingled with the program output, and that's because the `hello-world`
main function was returning before the thing is over.

I've added a `receive-result` call in it per worker so that it waits until
the end of the program before returning to the *REPL*, and that indeed fixes
it. A way to assert that is using the `time` macro, which was always
intermingled with the output before. It's fixed now:

~~~ lisp
CL-USER> (time (go-hello-world:hello-world 1000))
Hello World!
...
Hello World!
(GO-HELLO-WORLD:HELLO-WORLD 1000)
took 27,886 microseconds (0.027886 seconds) to run.
      1,593 microseconds (0.001593 seconds, 5.71%) of which was spent in GC.
During that period, and with 4 available CPU cores,
     23,246 microseconds (0.023246 seconds) were spent in user mode
     14,427 microseconds (0.014427 seconds) were spent in system mode
 4,272 bytes of memory allocated.
 10 minor page faults, 0 major page faults, 0 swaps.
(#<PROCESS lparallel kernel shutdown manager(62) [Reset] #x30200109F65D> ...)
CL-USER> 
~~~



## Conclusion

While *Go* language seems to bring very interesting things on the table,
such as better compilation units and tools, I still think that the
concurrency primitives at the core of it are easy to find in other places.
Which is a good thing, as it means we know they work.

That also means that we don't need to accept *Go* syntax as the only way to
properly solve that *concurrency* problem, I much prefer doing so with
*Common Lisp* (lack of?) syntax myself.


## Update

A previous version of this article was finished and published too quickly,
and the conclusion was made from a buggy version of the program. It's all
fixed now. Thanks a lot to people who contributed comments so that I could
fix it, and thanks again to *James M. Lawrence*
for [lparallel](http://lparallel.org/)!
