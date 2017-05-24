+++
date = "2011-04-23T10:30:00.000000+02:00"
title = "Emacs and PostgreSQL, PL line numbering"
tags = ["Emacs", "el-get", "pgsql-linum-format"]
categories = ["Emacs","el-get"]
thumbnailImage = "/img/old/emacs-pgsql-linum.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/emacs-pgsql-linum.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/04/23-emacs-and-postgresql-pl-line-numbering",
           "/blog/2011/04/23-emacs-and-postgresql-pl-line-numbering.html"]
+++

A while ago I've been fixing and publishing 
[pgsql-linum-format](https://github.com/dimitri/pgsql-linum-format) separately.
That allows to number 
`PL/whatever` code lines when editing from 
[Emacs](http://www.gnu.org/software/emacs/), and
it's something very useful to turn on when debugging.

The carrets on the 
*fringe* in the emacs window are the result of
`(setq-default indicate-buffer-boundaries 'left)` and here it's
just overloading the image somehow.  But the idea is to just 
`M-x linum-mode`
when you need it, at least that's my usage of it.

You can use 
[el-get](https://github.com/dimitri/el-get) to easily get (then update) this little 
`Emacs` extension.
