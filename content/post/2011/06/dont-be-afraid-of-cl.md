+++
date = "2011-06-20T00:15:00.000000+02:00"
title = "Don't be afraid of 'cl"
tags = ["Emacs"]
categories = ["Emacs","Emacs Tips"]
thumbnailImage = "/img/old/emacs-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/emacs-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/06/20-dont-be-afraid-of-cl",
           "/blog/2011/06/20-dont-be-afraid-of-cl.html"]
+++

In this 
[blog article](http://tsengf.blogspot.com/2011/06/confirm-to-quit-when-editing-files-from.html), you're shown a quite long function that loop through
your buffers to find out if any of them is associated with a file whose full
name includes 
`"projects"`.  Well, you should not be afraid of using 
`cl`:

~~~
(require 'cl)
(loop for b being the buffers
      when (string-match "projects" (or (buffer-file-name b) ""))
      return t)
~~~


If you want to collect the list of buffers whose name matches your test,
then replace 
`return t` by 
`collect b` and you're done.  Really, this 
`loop` thing
is worth learning.
