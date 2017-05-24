+++
date = "2013-01-09T11:07:00.000000+01:00"
title = "Lost in scope"
tags = ["Common-Lisp", "Scope", "Python", "Ruby", "Javascript"]
categories = ["Software Programming","Common Lisp"]
thumbnailImage = "/img/old/lambda.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/lambda.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/01/09-Lost-in-scope",
           "/blog/2013/01/09-Lost-in-scope.html"]
+++

Thanks to 
[Mickael](https://twitter.com/mickael/status/288795520179240962) on 
*twitter* I got to read an article about loosing scope
with some common programming languages. As the blog article 
[Lost in scope](https://my.smeuh.org/al/blog/lost-in-scope)
references 
*functional programming languages* and plays with both 
*Javascript*
and 
*Erlang*, I though I had to try it out with 
*Common Lisp* too.

<center>*Let's have fun with lambda!*</center>

So, here we go with a simple Common Lisp attempt. The 
*Lost in scope* article
begins with defining a very simple function returning a boolean value, only
true when it's not 
`monday`.


## Monday is special

Keep in mind that the following example has been choosen to be simple yet
offer a case of 
*lexical binding shadowing*. It looks convoluted. Focus on the
`day` binding.

~~~
(defparameter *days*
  '(monday tuesday wednesday thursday friday saturday sunday)
  "List of days in the week")

(defun any-day-but-monday? (day)
  "Returns a generalized boolean, true unless DAY is 'monday"
  (member day (remove-if (lambda (day) (eq day 'monday)) *days*)))
~~~


So as you can see, in 
*Common Lisp* we just get away with a list of symbols
rather than a string that we split to have a list of strings, or an array of
strings, as in the examples with 
*python* and 
*ruby*.

Now, the 
*generalized boolean* is either 
`nil` to mean false, or anything else
to mean 
`true`, and in that example the return value of 
[member](http://www.lispworks.com/documentation/HyperSpec/Body/a_member.htm) is a sub-list
that begins where the 
*member* was found:

~~~
CL-USER> (any-day-but-monday? 'monday)
NIL

CL-USER> (any-day-but-monday? 'tuesday)
(TUESDAY WEDNESDAY THURSDAY FRIDAY SATURDAY SUNDAY)
~~~


Oh, and as we work with 
*Common Lisp*, we're having a real 
[REPL](http://www.gigamonkeys.com/book/lather-rinse-repeat-a-tour-of-the-repl.html) where to play
directly with our code, no need to add 
*interactive* stanzas in the main
program text file just to be able to play with it. In 
[Emacs Slime](http://common-lisp.net/project/slime/) we just
use 
`C-M-x` on a 
*form* to have it available in the 
*REPL*, or 
`C-c C-l` to load the
whole file we're working on.

So, we see that 
*Common Lisp* scoping rules are silently doing the right thing
here. Within the 
[remove-if](http://www.lispworks.com/documentation/HyperSpec/Body/f_rm_rm.htm) call we define a 
*lambda* function taking a single
parameter called 
*day*. It so happens that this parameter is shadowing the
*any-day-but-monday?* function parameter, and that shadowing only happens in
the 
*lexical scope* of the 
*lambda* we are creating. For a detailed discussion
about that concept, I would refer you to the 
[Scope and Extent](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node43.html) chapter of
*Common Lisp the Language, 2nd Edition*.

In 
*Common Lisp* we have both 
*lexical scope* and 
*dynamic extents*, and a
variable defined with 
*defparameter* or 
*defvar* or that you otherwise 
[declare](http://www.lispworks.com/documentation/HyperSpec/Body/s_declar.htm)
*special* will have a 
*dynamic extent*. Hence this section title.


## Closures

Now, the 
[lost in scope](https://my.smeuh.org/al/blog/lost-in-scope) article tries some more at finding a solution around
the scoping rules of the 
*python* and 
*ruby* languages, where the developer can
not easily instruct the language about the scoping rules he wants to be
using in a case by case way, as far as I can see.

First, let's reproduce the problem by using a single variable that we bind
in all the closures. Those are called 
*callbacks* in the original article, so
I've kept using that name here.

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/callback.jpg" >}}
</center>

~~~
(defparameter *callbacks-all-sunday*
    (loop
       for day in *days*
       collect (lambda () day))
  "loop binds DAY only once")
~~~


In that example, there's only a single variable day that we reuse throughout
the 
*loop* construct, so that when the loop ends, we have a list of closures
all refering to the same variable, and this variable, by the end of the
loop, has 
`sunday` as its value.

~~~
CL-USER> (mapcar #'funcall *callbacks-all-sunday*)
(SUNDAY SUNDAY SUNDAY SUNDAY SUNDAY SUNDAY SUNDAY)
~~~



## Closures, take 2

Now, the way to have what we want here, that is a list of closures each
having its own variable.

~~~
(defparameter *callbacks*
  (mapcar (lambda (day)
	    ;; for each day, produce a separate closure
	    ;; around its own lexical variable day
	    (lambda () day))
	  *days*)
  "A list of callbacks to return the current day...")
~~~


And there we go:

~~~
CL-USER> (mapcar #'funcall *callbacks*)
(MONDAY TUESDAY WEDNESDAY THURSDAY FRIDAY SATURDAY SUNDAY)
~~~



## Conclusion

Scoping rules are very important in any programming language, functional or
not, and must be well understood by programmers. I find that once again,
that topic has received a very deep thinking in 
*Common Lisp*, and the
language is giving all the options to its developers.

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/scope.png" >}}
</center>

<center>*What are your language of choice scoping rules?*</center>

I want to stress that in 
*Common Lisp* the scope rules are very clearly
defined in the standard documentation of the language. For instance, 
*defun*
and 
*let* both introduce a lexical binding, 
*defvar* and 
*defparameter* introduce
a 
*dynamic variable*.

Also, as a user of the language you have the ability to 
*declare* any variable
as being 
*special* in order to introduce yourself a 
*dynamic variable*. In 
`C` you
can declare some variables as being 
*static*, which is something else and
frown with a very different set of problems.
