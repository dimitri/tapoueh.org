+++
date = "2011-07-29T10:00:00.000000+02:00"
title = "Emacs ANSI colors"
tags = ["Emacs"]
categories = ["Emacs","Emacs Tips"]
thumbnailImage = "/img/old/emacs-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/emacs-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/07/29-emacs-ansi-colors",
           "/blog/2011/07/29-emacs-ansi-colors.html"]
+++

[Emacs](http://tapoueh.org/emacs/index.html) comes with a pretty good implementation of a terminal emulator, 
`M-x
term`.  Well not that good actually, but given what I use it for, it's just
what I need.  Particulary if you add to that my 
[cssh](http://tapoueh.org/emacs/cssh.html) tool, so that
connecting with 
`ssh` to a remote host is just a 
`=C-= runs the command
cssh-term-remote-open` away, and completes on the host name thanks to
`~/.ssh/known_hosts`.

Now, a problem that I still had to solve was the colors used in the
terminal.  As I'm using the 
*tango* color theme for emacs, the default 
*ANSI*
palette's blue color was not readable.  Here's how to fix that:

~~~
(require 'ansi-color)
   (setq ansi-color-names-vector
         (vector (frame-parameter nil 'background-color)
    	       "#f57900" "#8ae234" "#edd400" "#729fcf"
    	       "#ad7fa8" "cyan3" "#eeeeec")
         ansi-term-color-vector ansi-color-names-vector
         ansi-color-map (ansi-color-make-color-map))
~~~


Now your colors in an emacs terminal are easy to read, as you can see:

    

{{< image classes="fig50 fancybox dim-margin" src="/img/old/emacs-tango-term-colors.png" >}}


Hope you enjoy!  
