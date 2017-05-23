+++
date = "2010-12-20T16:45:00.000000+01:00"
title = "el-get 1.1, with 174 recipes"
tags = ["Emacs", "debian", "el-get", "release"]
categories = ["debian"]
thumbnailImage = "/img/old/debian-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/debian-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/12/20-el-get-11-with-174-recipes",
           "/blog/2010/12/20-el-get-11-with-174-recipes.html"]
+++

Yes, you read it well, 
[el-get](https://github.com/dimitri/el-get) currently 
*features* 
`174` 
[recipes](https://github.com/dimitri/el-get/tree/master/recipes), and is now
reaching the 
`1.1` release. The reason for this release is mainly that I have
two big chunks of code to review and the current code has been very stable
for awhile. It seems better to do a release with the stable code that exists
now before to shake it this much. If you're wondering when to jump in the
water and switch to using 
*el-get*, now is a pretty good time.


## New source types

We now have support for the 
[pacman](http://www.archlinux.org/pacman/) package management for 
[archlinux](http://www.archlinux.org/), and a
way to handle a different package name in the recipe and in the
distribution. We also have support for 
[mercurial](http://mercurial.selenic.com/) and 
[subversion](http://subversion.tigris.org/) and 
[darcs](http://darcs.net/).

Also, 
[apt-get](http://wiki.debian.org/Apt) will sometime prompt you to validate its choices, that's the
infamous 
*Do you want to continue?* prompt. We now handle that smoothly.


## (el-get 'sync)

In 
`1.1`, that really means 
*synchronous*. That means we install one package
after the other, and any error will stop it all. Before that, it was an
active wait loop over a parallel install: this option is still available
through calling 
`(el-get 'wait)`.


## No more *failed to install*

Exactly. This error you may have encountered sometime is due to trying to
install a package over a previous failed install attempt (network outage,
disk full, bad work-in-progress recipe, etc). After awhile in the field it
was clear that no case where found where you would regret it if 
[el-get](https://github.com/dimitri/el-get) just
did removed the previous failed installation for you before to go and
install again, as aked. So that's now automatic.


## Featuring an overhauled :build facility

The 
`build` commands can now either be a list, as before, or some that we
*evaluate* for you. That allows for easier to maintain 
*recipes*, and here's an
exemple of that:

~~~
(:name distel
       :type svn
       :url "http://distel.googlecode.com/svn/trunk/"
       :info "doc"
       :build `,(mapcar
                 (lambda (target)
                   (concat "make " target " EMACS=" el-get-emacs))
                 '("clean" "all"))
       :load-path ("elisp")
       :features distel)
~~~


As you see that also allows for maintainance of multi-platform build
recipes, and multiple emacs versions too. It's still a little too much on
the 
*awkward* side of things, though, and that's one of the ongoing work that
will happen for next version.


## Misc improvements

We are now able to 
`byte-compile` your packages, and offer some more hooks
(
`el-get-init-hooks` has been asked with a nice usage example). There's a new
`:localname` property that allows to pick where to save the local file when
using 
`HTTP` method for retrieval, and that in turn allows to fix some
*recipes*.

~~~
(:name xcscope
       :type http
       :url "http://cscope.cvs.sourceforge.net/viewvc/cscope/cscope/contrib/xcscope/xcscope.el?revision=1.14&content-type=text%2Fplain"
       :localname "xscope.el"
       :features xcscope)
~~~


Oh and you even get 
`:before` user function support, even if needing it often
shows that you're doing it in a strange way. More often than not it's
possible to do all you need to in the 
`:after` function, but this tool is
there so that you spend less time on having a working environment, not more,
right? :)


## Switch notice

All in all, if you're already using 
[el-get](https://github.com/dimitri/el-get) you should consider switching to
`1.1` (by issuing 
`M-x el-get-update` of course), and if you're hesitating, just
join the fun now!
