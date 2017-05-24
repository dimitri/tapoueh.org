+++
date = "2013-07-08T13:34:00.000000+02:00"
title = "Emacs Muse meets Common Lisp"
tags = ["Common-Lisp", "Emacs", "Muse"]
categories = ["Software Programming","Emacs Lisp"]
thumbnailImage = "/img/old/Emacs-Muse.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/Emacs-Muse.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/07/08-Muse-blog-compiler",
           "/blog/2013/07/08-Muse-blog-compiler.html"]
+++

This blog of mine is written in the very good 
[Emacs Muse](http://mwolson.org/projects/EmacsMuse.html) format, that I find
much more friendly to writing articles than both 
[org-mode](http://orgmode.org/) and 
[markdown-mode](http://jblevins.org/projects/markdown-mode/)
that I both use in a regular basis too. The main think that I like in Muse
that those two others lack is the support for displaying images inline.

<center>*Here's what it looks like to edit with Emacs Muse*</center>


## The Muse publishing system

The idea is that you edit 
`.muse` files in Emacs then use a command to 
*publish*
your file to some format like HTML. You can also publish a whole project,
and then enjoy a fully static website that you can deploy on some URL.

The drawback of using the Muse format here is to do with the associated
tooling. I didn't take time to study Muse Emacs Lisp sources and probably I
should have, as I found myself the need to add support for 
*tags* and per-tag
*rss* support. What I did then was using Muse provided 
*hooks* so that my code
gets run here and there.

With my additions, though, publishing a single article took a painful time,
something around 30 seconds for the main page of it and then about as much
(sometimes more) to publish the project: previous and next articles
sections, tag files, rss files.


## from Emacs Lisp to Common LIsp
<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/lisp-is-different.jpg" >}}
</center>

<center>*and Common Lisp is different from Emacs Lisp*</center>

When I realized that my motivation to writing new blog articles was going so
low that I wasn't doing that anymore, I raised the priority of fixing my
blog setup enough so that I would actually do something about it. And then
decided it would actually be a good excuse for another 
*Common Lisp* project.

It's available at 
[git.tapoueh.org](http://git.tapoueh.org/?p=tapoueh.org.git;a=summary) with my usual choice of licencing, the
[WTFPL](http://www.wtfpl.net/).

I've been using 
[esrap](http://nikodemus.github.io/esrap/) to write a Muse parser along with 
[5am](http://common-lisp.net/project/fiveam/) to test it
extensively. It turned out not an easy project to just parse the articles,
but thanks to 
[cl-who](http://weitz.de/cl-who/) (that stands for 
`with-html-output`) the output of the
parser is a very simple 
*nested list* data structure.

In the 
**Emacs Lisp** Muse system there was a cool idea that you could embed
(compile time) dynamic sections of 
`<lisp>(code)</lisp>` in the templates, so
I've kept that idea and implemented a 
*Server Side Include* facility.

The idea has then been to build a dynamic website without any level of
caching at all so that anytime you reload a page its muse source file is
parsed and intermediate representation is produced. Then the 
**SSI** kicks with
adding the 
*header* and 
*footer* around the main article's body, and replacing
any embedded code calls with their results. Finally,
`with-html-output-to-string` is used to return an HTML string to the browser.

With all that work happening at run-time, one would think that the
performances of the resulting solution would be awful. Well in fact not at
all, as you can see:

~~~
$ time curl -s http://localhost:8042/blog/2013/07/08-Muse-blog-compiler > /dev/null
real	0m0.081s
~~~


And then some quick measurements of time spent to parse all the articles
I've ever published here:

~~~
TAPOUEH> (time (length (find-blog-articles *blog-directory*
					   :parse-fn #'muse-parse-article)))
Evaluation took:
  0.484 seconds of real time
  0.486381 seconds of total run time (0.415208 user, 0.071173 system)
  [ Run times consist of 0.089 seconds GC time, and 0.398 seconds non-GC time. ]
  100.41% CPU
  1,113,697,675 processor cycles
  206,356,848 bytes consed
  
181
~~~


So it takes about 
`80 ms` to render a full dynamic page. That's way better
than what I wanted to achieve!


## the static compiler

That said, we're talking about the ability to render about 
`12` pages per
second, which is not something acceptable as-is for production use. And
given the expected ratio of reads and writes, there's no good reason to
publish a dynamic website, so the next step here is to build a 
*static
website compiler*.

And here's how it looks like now:

~~~
TAPOUEH> (compile-articles)
parsed 199 docs in 0.627s
parsed chapeau of 172 blog articles in 0.114s
compiled the home page in 0.015s
compiled the tags cloud in 0.005s
compiled the blog archives page in 0.085s
compiled 199 documents in 12.333 secs
compiled 56 blog indexes in 1.721s
compiled 64 tag listings in 1.073s
compiled 6 rss feeds in 3.806s
compiled the sitemap in 0.021s
~~~


So it takes about 20 seconds to publish again the whole website. The reason
why it's ok for me not to optimize this yet is because I've also been
changing the CSS and HTML parts of the website, and each time the header or
the parser is changed, or even some SSI function's output, then I need to
compile the whole set of files anyway.


## The `displaying-time` macro
<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/lisplogo_fancy_256.png" >}}
</center>

While building this little Common Lisp project, I've added to my still very
little toolbelt a macro that I like. Here's how to use it:

~~~
(let* ((all-documents
	  (displaying-time ("parsed ~d docs in ~ds~%" (length result) timing)
	    (find-muse-documents)))
	 (blog-articles
	  (displaying-time ("parsed chapeau of ~d blog articles in ~ds~%"
			    (length result) timing)
	    (find-blog-articles *blog-directory*))))

    (displaying-time ("compiled the home page in ~ds~%" timing)
      (compile-home-page :documents blog-articles :verbose verbose))
    ...)
~~~


And here's the code I wrote to have this macro:

~~~
(defun elapsed-time-since (start)
  "Return how many seconds ticked between START and now"
  (/ (- (get-internal-real-time) start)
     internal-time-units-per-second))

(defmacro timing (&body forms)
  "return both how much real time was spend in body and its result"
  (let ((start (gensym))
	(end (gensym))
	(result (gensym)))
    `(let* ((,start (get-internal-real-time))
	    (,result (progn ,@forms))
	    (,end (get-internal-real-time)))
       (values
	,result
	(float (/ (- ,end ,start) internal-time-units-per-second))))))

(defun replace-symbols-recursively (code symbols)
  "Walk CODE to replace symbols as given in the SYMBOLS alist."
  (loop
     for s-exp in code
     when (listp s-exp) collect (replace-symbols-recursively s-exp symbols)
     else collect (if (symbolp s-exp)
		      (destructuring-bind (before . after)
			  (or (assoc s-exp symbols) (cons nil nil))
			(if before after s-exp))
		      s-exp)))

(defmacro displaying-time ((fmt &rest bindings) &body forms)
  "display on *standard-output* how much time it took to execute body"
  (let* ((result   (gensym))
	 (timing   (gensym))
	 (bindings
	  (replace-symbols-recursively bindings `((result . ,result)
						  (timing . ,timing)))))
    `(multiple-value-bind (,result ,timing)
	 (timing ,@forms)
       (format t ,fmt ,@bindings)
       ,result)))
~~~


Note that I already had the 
`timing` macro and just used it as is.
