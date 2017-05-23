+++
date = "2011-08-06T14:58:00.000000+02:00"
title = "Emacs Startup"
tags = ["Emacs", "el-get"]
categories = ["Emacs","el-get"]
thumbnailImage = "/img/old/el-get.big.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/el-get.big.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/08/06-emacs-startup-notification",
           "/blog/2011/08/06-emacs-startup-notification.html"]
+++

Using 
[Emacs](http://www.gnu.org/software/emacs/) we get to manage a larger and larger setup file (either 
`~/.emacs`
or 
`~/.emacs.d/init.el`), sometime with lots of dependencies, and some
sub-files thanks to the 
`load` function or the 
`provide` and 
`require` mechanism.

Some users are even starting Emacs often enough for the startup time to be a
concern.  With an 
`emacs-uptime` (yes it's a command, you can 
`M-x
emacs-uptime`) of days to weeks (
`10 days, 17 hours, 45 minutes, 34 seconds` as
of this writing), it's not something I really care about much.

But I know that some 
[el-get](http://tapoueh.org/emacs/el-get.html) users still do care, and will use 
`el-get-is-lazy`
and do all their Emacs tweaking as 
`eval-after-load` blocks.  Trying to have
an idea of how much a 
*worst case* startup with 
[el-get](http://www.emacswiki.org/emacs/el-get) is, I have added the
following piece of 
`elisp` at the very end of my startup code:

~~~
(defun dim:notify-startup-done ()
  " notify user that Emacs is now ready"
  (el-get-notify
   "Emacs is ready."
   (format "The init sequence took %g seconds."
	   (float-time (time-subtract after-init-time before-init-time)))))

(add-hook 'after-init-hook 'dim:notify-startup-done)
~~~


The 
`el-get-notify` function will adapt and either use the dbus implementation
from Emacs 24, or 
[notify.el](http://www.emacswiki.org/emacs/notify.el) from 
[EmacsWiki](http://www.emacswiki.org/) (just 
`M-x el-get-install` it if
you need it), or will use its own implementation of an Emacs 
[Growl](http://growl.info/) client
(it's about 5 lines long), and baring all of that will use the 
`message`
function.

The reason I say 
*worst case* is that I have a lot of packages to initialize
at startup, and that I did absolutely no effort for this initializing to be
quick.  Still, my Emacs setup is taking about 20 seconds to boot.  Pretty
good I would say, for a weekly operation.
