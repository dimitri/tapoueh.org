+++
date = "2010-08-09T15:35:00.000000+02:00"
title = "el-get and dim-switch-window status update"
tags = ["Emacs", "el-get", "release", "switch-window", "rcirc"]
categories = ["Projects","switch-window"]
thumbnailImage = "/img/old/el-get.big.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/el-get.big.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/08/09-el-get-and-dim-switch-window-status-update",
           "/blog/2010/08/09-el-get-and-dim-switch-window-status-update.html"]
+++

Thanks to you readers of 
[Planet Emacsen](http://planet.emacsen.org/) taking the time to try those pieces
of emacs lisp found in my blog, and also the time to comment on them, some
bugs have been fixed, and new releases appeared.

[el-get](http://tapoueh.org/projects.html#sec20) had some typo kind of bug in its support for 
`apt-get` and 
`fink`
packages, and I managed to break the 
`elpa` and 
`http` support when going 
*all
asynchronous* by forgetting to update the call convention I'm using. Fixing
that, I also switched to using 
`url-retrieve` so that the 
`http` support also is
*asynchronous*. That makes the version 
`0.5`, available on 
[emacswiki el-get](http://www.emacswiki.org/emacs/el-get.el)
page.

Meanwhile 
[dim-switch-window.el](http://tapoueh.org/projects.html#sec19) got some testers too and got updated with a
nice fix, or so I think. If you're using it with a small enough emacs frame,
or some very little windows in there, you'd have noticed that the number get
so big they don't fit anymore, and all you see while it's waiting for your
window number choice is... blank windows. Not very helpful. Thanks to the
following piece of code, that's no longer the case as of the current
version, available on 
[emacswiki switch-window](http://www.emacswiki.org/emacs/switch-window.el) page.

In short, where I used to blindly apply 
`dim:switch-window-increase` on the
big numbers to display, the code now checks that there's enough room for it
to get there, and adjust the 
*increase* level scaling it down if
necessary. Very simple, and effective too:

~~~
(with-current-buffer buf
      (text-scale-increase 
       (if (> (/ (float (window-body-height win)) 
		 dim:switch-window-increase)
	      1)
	   dim:switch-window-increase
	 (window-body-height win)))
      (insert "\n\n    " (number-to-string num)))
~~~


Centering the text in the window's width is another story entirely, as the
`text-scale-increase` ain't linear on this axis. I'd take any good idea,
here's what I'm currently at, but it's not there yet:

~~~
(with-current-buffer buf
      (let* ((w (window-width win))
	     (h (window-body-height win))
	     (increased-lines (/ (float h) dim:switch-window-increase))
	     (scale (if (> increased-lines 1) dim:switch-window-increase h))
	     (lines-before (/ increased-lines 2))
	     (margin-left (/ w h) ))
	;; increase to maximum dim:switch-window-increase
	(text-scale-increase scale)
	;; make it so that the hyuge number appears centered
	(dotimes (i lines-before) (insert "\n"))
	(dotimes (i margin-left)  (insert " "))
	(insert (number-to-string num))))
~~~


So, if you're using one or the other (both?) of those utilities, update your
local version of them!

Note: I also fixed a but in 
[rcirc-groups](http://github.com/dimitri/rcirc-groups) this week-end, but I'll talk about
it in another entry, if I may.
