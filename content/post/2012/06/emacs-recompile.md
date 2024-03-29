+++
date = "2012-06-01T18:45:00.000000+02:00"
title = "M-x recompile"
tags = ["Emacs"]
categories = ["Emacs","Emacs Tips"]
thumbnailImage = "/img/old/emacs-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/emacs-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2012/06/01-emacs-recompile",
           "/blog/2012/06/01-emacs-recompile.html"]
+++

A friend of mine just asked me for advice to tweak some Emacs features, and
I think that's really typical of using Emacs: rather than getting used to
the way things are shipped to you, when using Emacs, you start wanting to
adapt the tools to the way you want things to be working instead. And you
can call that the awesome!

In this case we're talking about the 
`M-x compile` and 
`M-x recompile`
functions. My friend bound the former to 
`<f11>` and wanted that 
`C-u f11` do a
recompile with the exact same command line as the previous 
`compile` command.

Well, to be honest, I didn't know about 
`M-x recompile` until after I wrote
the following function, made to trigger another 
`compile` with the last
command used if using 
`C-u`.

~~~
(defvar cyb-compile-last-command nil)
(defvar cyb-compile-command-history nil)

(defun cyb-compile (arg)
  "Compile with given command, optionally recompile with last command"
  (interactive "P")
  (if arg
      (progn
	;; arg given: compile with last command
	(unless cyb-compile-last-command
	  (error "Can't recompile yet, no known last command"))
	(compile cyb-compile-last-command))
    ;; else branch, no arg given, ask for a command
    (let ((command
	   (read-string
	    "Compile with command: "
            "make -k" 'cyb-compile-command-history "make -k")))
      (setq cyb-compile-last-command command)
      (compile command))))

(global-set-key (kbd "<f11>") 'cyb-compile)
~~~


With that little 
*Emacs Lisp* code we're driving Emacs the way we want to be
working, and that's great! You can see it was a 
*quick hack* in that if you
wanted to use the function non interactively it would still prompt for the
command to use to compile, when 
*Emacs Lisp* 
`interactive` special form would
allow us to implement something way smarter here. Also if we wanted to spend
some more time on that feature, we should probably tweak the 
*error* condition
to be asking for the command rather than just complaining, that would
certainly be more useful.

Exercise left to the reader, rewrite using 
`recompile` rather than reinventing
it in a hurry! Beware of 
`call-interactively` though. Oh and fix the
aforementioned infelicities, too.

To conclude, we see that writing 
*Emacs Lisp* code to fix a usability problem
in a hurry is a great force of Emacs, and that we're provided with the
necessary tool set so as to be able to reach completeness if we wanted to do
so.
