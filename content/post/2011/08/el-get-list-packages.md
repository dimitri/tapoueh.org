+++
date = "2011-08-18T18:10:00.000000+02:00"
title = "el-get-list-packages"
tags = ["Emacs"]
categories = ["Emacs","Emacs Tips"]
thumbnailImage = "/img/old/emacs-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/emacs-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/08/18-el-get-list-packages",
           "/blog/2011/08/18-el-get-list-packages.html"]
+++

From the first days of 
[el-get](../../../emacs/el-get.html) is was quite clear for me that we would reach
a point where users would want a nice listing including descriptions of the
packages, and a 
*major mode* allowing you to select packages to install,
remove and update.  It was also quite clear that I was not much interested
into doing it myself, even if I would appreciate having it done.

Well, the joy of Open Source & Free Software (pick your own poison).
[jglee1027](https://github.com/jglee1027) is this 
*GitHub* guy who did offer an implementation of said
facility, and who added descriptions for almost all of the now 
`402` recipes
that we have included with 
[el-get](../../../emacs/el-get.html).

Here's an image of what you get:

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/emacs-el-get-list-packages.png" >}}
</center>

The packages with no description are fetched by 
`M-x el-get-emacswiki-refresh`
which will not download all 
[emacswiki](http://emacswiki.org) content locally just so that it can
parse the scripts's header and have a local description.  Maybe it's time to
ask for another page over there like 
[emacswiki page index](http://www.emacswiki.org/cgi-bin/wiki?action=index;match=%5C.(el%7Ctar)(%5C.gz)%3F%24) but containing the
first line too.

For recipes we offer, this first line often looks like the following:

~~~
;;; 123-menu.el --- Simple menuing system, reminiscent of Lotus 123 in DOS
~~~


Of course some files over there are not following the stanza, but that would
be good enough already.

All in all, I hope you enjoy 
`M-x el-get-list-packages`!
