+++
date = "2010-03-04T13:33:00.000000+01:00"
title = "Emacs Muse hacking"
tags = ["Emacs", "Muse"]
categories = ["Software Programming","Emacs Lisp"]
thumbnailImage = "/img/old/emacs-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/emacs-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/03/04-emacs-muse-hacking",
           "/blog/2010/03/04-emacs-muse-hacking.html"]
+++

Now you know what piece of software is used to publish this blog. I really
like it, the major mode makes it a great experience to be using this tool,
and the fact that you produce the 
`HTML` and 
`rsync` it all from within Emacs
(
`C-c C-p` then 
`C-c C-r` with some easy 
[elisp code](http://git.tapoueh.org/?p=tapoueh.org.git;a=blob;f=dim-muse.el;hb=HEAD)) is a big advantage as far
as I'm concerned. No need to resort to 
`shell` and 
`Makefile`.

What's new here is that I missed the 
*one page per article* trend that other
blog software propose, and the blog entries index too. I didn't want to
invest time into hacking Muse itself, that was my excuse for accepting the
situation. But I finally took a deeper look at the 
[Emacs Muse Manual](http://mwolson.org/static/doc/muse/Style-Elements.html#Style-Elements), and
found out about the 
`:after` and 
`:final` functions.

Those two function will get run while in the output buffer, the 
`HTML`
formatted one. With the 
`:after` function, it's still possible to edit the
buffer content, for example to add a mini index to previous articles,
whereas with the 
`:final` function the buffer is 
`read-only` and already written
to disk, so it's to late to edit it. Still it's possible to cut it in pieces
and write a new file per article you find in there.

The code to realize my wishes is 
[available](http://git.tapoueh.org/?p=tapoueh.org.git;a=summary) but has not been edited with
customisation in mind, so to use it you will have to edit some places rather
than just 
`setq` some 
`defcustom`. Well, if I have demand, I'll generalize the
code and share it on 
[Emacs Wiki](http://www.emacswiki.org/) and 
[ELPA](http://tromey.com/elpa/). Meanwhile, happy hacking!
