+++
date = "2010-07-25T13:25:00.000000+02:00"
title = "dim-switch-window.el"
tags = ["Emacs", "el-get", "switch-window"]
categories = ["Projects","switch-window"]
thumbnailImage = "/img/old/emacs-switch-window.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/emacs-switch-window.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/07/25-dim-switch-windowel",
           "/blog/2010/07/25-dim-switch-windowel.html"]
+++

So it's Sunday and I'm thinking I'll get into 
`el-get` sometime later. Now is
the time to present 
`dim-switch-window.el` which implements a 
*visual* 
`C-x o`. I
know of only one way to present a 
*visual effect*, and that's with a screenshot:


So as you can see, it's all about showing a 
*big* number in each window,
tweaking each window's name, and waiting till the user press one of the
expected key — or timeout and stay on the same window as before 
`C-x o`. When
there's only 1 or 2 windows displayed, though, the right thing happen and
you see no huge number (in the former case, nothing happens, in the latter,
focus moves to the other window).

The code for that can be found on 
[emacswiki](http://www.emacswiki.org/) under the name
[switch-window.el](http://www.emacswiki.org/emacs/switch-window.el). Hope you'll find it useful!
