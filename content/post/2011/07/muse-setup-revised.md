+++
date = "2011-07-05T19:55:00.000000+02:00"
title = "Muse setup revised"
tags = ["Emacs", "Muse"]
categories = ["Software Programming","Emacs Lisp"]
thumbnailImage = "/img/old/emacs-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/emacs-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/07/05-muse-setup-revised",
           "/blog/2011/07/05-muse-setup-revised.html"]
+++

Most of you are probably reading my posts directly in their 
`RSS` reader tools
(mine is 
[gnus](http://www.gnus.org/) thanks to the 
[Gwene](http://gwene.org/) service), so you probably missed it, but I
just 
*pushed* a whole new version of 
[my website](http://tapoueh.org), still using 
[Emacs Muse](https://github.com/alexott/muse) as the
engine.

My setup is tentatively called 
[tapoueh.el](../../../tapoueh.el.html) and browsable online.  It consists
of some tweaks on top of Muse, so that I can enjoy 
[tags](../../../tags/index.html) and proper 
[rss](../../../rss/)
support.  By 
*proper*, I mean that I want to be able to produce as many 
*topic*
`RSS` 
*feeds* from a single 
*blog*, and thanks to the 
*tags* support that's now what
I have.

The 
`RSS` handling and the tagging system are adhoc code, and this very
article begins like this:

~~~
#author Dimitri Fontaine
#title  Muse setup revised
#date   20110705-19:55
#tags   Emacs Muse
~~~


All the information for the site navigation are taken from there, and at
long last the 
`RSS` I publish now contains proper 
`URLs` without abusing
[anchors](../../../blog.dim.html), as in the previous link which is a compatibility page in case you
had some bookmarks.  The compat only works with javascript (did you know
that 
*anchors* are not part of the 
`URL` that is sent to the server, so that you
can't apply 
`RedirectMatch` or other tweaks?), but all it needs is 
*2 lines of
code*, so I guess that's not so bad.

~~~
var anchor = window.location.hash;
document.location.href=document.getElementById(anchor).href;
~~~


I hope you like the new setup as much as I do, even if I'm left with some
debugging to do.  That's the price to pay for doing it yourself I guess.
But I still don't know of a ready to use solution (as in 
*off the shelf*) that
meet my criteria for web publishing.  More on that topic another time.
