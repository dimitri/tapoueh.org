+++
date = "2011-09-16T14:13:00.000000+02:00"
title = "el-get-3.1"
tags = ["Emacs", "el-get", "release"]
categories = ["Emacs","el-get"]
thumbnailImage = "/img/old/el-get.big.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/el-get.big.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/09/16-el-get-3",
           "/blog/2011/09/16-el-get-3.html",
           "/blog/2011/09/16-el-get-3.1",
           "/blog/2011/09/16-el-get-3.1.html"]
+++

The 
[el-get](https://github.com/dimitri/el-get) project releases its new stable version, 
`3.1`. This new release
fixes bugs, add a host of new recipes (we have 420 of them and counting) and
some nice new features too.  You really want to upgrade.


# New features

Among the features you will find dependencies management and 
`M-x
el-get-list-packages`, that you should try as soon as possible.  Of course,
don't miss 
`M-x el-get-self-update` that eases the process somehow.

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/emacs-el-get-list-packages.png" >}}
</center>

This shows the result of 
`M-x el-get-list-packages`.  The packages that don't
have a description are the one from 
[emacswiki](http://www.emacswiki.org/cgi-bin/wiki?action=index;match=%5C.(el|tar)(%5C.gz)%3F%24) that doesn't provide a listing
of the filename 
*and* the first line of the file (it usually follows the
format 
`;;; filename.el --- description here`).  As we don't want to mirror
the website just to be able to provide descriptions, we just don't have them
now.

Another nice new feature, contributed by a user that wanted to self-learn
[elisp](http://www.gnu.org/software/emacs/manual/html_node/elisp/index.html), is the 
`el-get-user-package-directory` support.  Just place in there
some 
`init-my-package.el` files, and when 
*el-get* wants to init the 
`my-package`
package, it will load that file for you.  That helps managing your setup,
and I'm already using that in my own 
`~/.emacs.d/` repository.


# Upgrading

The upgrading is to be done with some care, though, because you need to edit
your packaging setup.  The 
`el-get-sources` variable used to be both where to
setup extra recipes and the list of packages you want to have installed, and
several people rightfully insisted that I should change that.  I've been
slow to be convinced, but there it is, they were right.

So now, 
[el-get](http://www.emacswiki.org/emacs/el-get) works from the current status of packages and will init all
those packages you have installed.  Which means that you just 
`M-x
el-get-install` a package and don't think about it anymore.  If you need to
override this behavior, it's still possible to do so by specifying the whole
list of packages you want initialized (and installed if necessary) on the
`(el-get 'sync ...)` call.

That later setup is useful if you want to share your el-get selection on
several machines.
