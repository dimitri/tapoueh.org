+++
date = "2010-07-26T11:55:00.000000+02:00"
title = "dim-switch-window.el: fixes"
tags = ["Emacs", "switch-window"]
categories = ["Projects","switch-window"]
thumbnailImage = "/img/old/emacs-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/emacs-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/07/26-dim-switch-windowel-fixes",
           "/blog/2010/07/26-dim-switch-windowel-fixes.html"]
+++

Thanks to amazing readers of 
[planet emacsen](http://planet.emacsen.org/), two annoyances of
[switch-window.el](http://www.emacswiki.org/emacs/switch-window.el) have already been fixed! The first is that handling of 
`C-g`
isn't exactly an option after all, and the other is that you want to avoid
the buffer creation in the simple cases (1 or 2 windows only), because it's
the usual case.

I've received code to handle the second case, that I mostly merged. Thanks a
lot guys, the new version is on 
[emacswiki](http://wwww.emacswiki.org) already!
