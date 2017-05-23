+++
date = "2012-08-28T11:43:00.000000+02:00"
title = "El-Get 4.1 is out"
tags = ["Emacs", "el-get", "release"]
categories = ["Emacs","el-get"]
thumbnailImage = "/img/old/el-get.big.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/el-get.big.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2012/08/28-el-get-new-stable-release",
           "/blog/2012/08/28-el-get-new-stable-release.html"]
+++

Please welcome the new stable version of 
[El-Get](https://github.com/dimitri/el-get#readme), the much awaited 
`version
4.1` has now been branched for your pleasure. It's packed with lots of
features to make your life easy, comes with a 
*Info* documentation book and
even has a 
*logo*. That's no joke, I found one, at least:



## Why El-Get is relevant

Emacs 24.1 is the first release that includes 
`package.el`, and it even allows
the user to setup several sources where to fetch packages. Those sources,
such as 
[Marmalade](http://marmalade-repo.org/), are hosting lots of third party code for Emacs.
`package.el` makes it easy to 
*install* (partly) those software.

This is a very fine way of getting extra features in your Emacs
installation, and one that is supported out of the box. For a 
*package* to be
listed, its sources need to be prepared, and you need to rely on the central
website you now depend on to be up and running and accessible.

El-Get is all about allowing you to easily cope with the still vast majority
of Emacs Lisp extensions you can find out there, that is non packaged code
that is only available on some more or less mainstream 
*distribution method*,
ranging from 
[EmacsWiki](http://emacswiki.org/) to 
[github](http://github.com/) including 
*bare HTTP* personal hosting.

With El-Get, you fetch the package where it's located. There's no need for a
central server to host packaged and released software, and it's easy to
share your findings with friends, or even to 
*publish* any 
`elisp` code you
write.

El-Get will also take care of final steps that 
`package.el` did choose not to
support, such as including 
*Info* material in your info browser (remember 
`C-h
i runs the command info`?), running 
`./configure && make` for you, 
*byte
compiling* the sources you just retrieved, adding the necessary 
[autoload](http://www.gnu.org/software/emacs/emacs-lisp-intro/html_node/Autoload.html)
support, etc.

And of course one of the 
*methods* supported by El-Get is 
`ELPA`, known as 
*Emacs
Lisp Package Archive* and implemented by 
`package.el`.

So definitely, you typically want both 
`ELPA` and 
[El-Get](http://github.com/dimitri/el-get).


## El-Get 4.1 Changelog Summary

The new El-Get release is packed with features. It really is. I will only
list some of them now:

  - Plenty new recipes, we now have `590` of them managed in the El-Get source

   repository itself, and El-Get will download the current 
[EmacsWiki](http://emacswiki.org/) list of
   
`emacs lisp` files at install time too.

  - The default installation and usage has been simplified a lot.

  - More options are provided to setup El-Get packages, see

   
`el-get-user-package-directory` for example.

  - Part of the simplification, `el-get-sources` has been revisited and now

   serves only one goal.

  - We dropped `(el-get 'wait)` which was a misconception and had been broken

   for a long time in the development version of El-Get.

  - We made improvements in the error handling and in dealing with some

   corner cases that still happen often enough for users to report them.
   Please continue reporting them!

  - More caching is done, with a better dependency tracking and status

   management.

  - Enhanced notification support, from `DBUS` to `growl`.

  - Support for *checksums* with a lot of *source types.*

  - Completing our `git` support, *shallow* clones and *submodules* are there.

  - Better support for `github` including the `zip` and `tar` releases.

  - Ability to reload a package when it's been *updated*.

  - *Moar* features

And most importantly, El-Get documentation is now almost complete and comes
in the nice 
*Info* format I know you've been expecting for so long!


## Using El-Get

Here's a quick summary of what using El-Get is like, for a new user in 4.1.
If you're already using El-Get see the section about upgrading. To install
El-Get you need to paste those lines to your 
`*scratch*` buffer then hit 
`C-j`
after the last closing parenthesis:

~~~
(url-retrieve
 "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
 (lambda (s)
   (goto-char (point-max))
   (eval-print-last-sexp)))
~~~


Then you can try 
`M-x el-get-list-packages` and browse through more than 
`2000`
available packages. Mark the ones you want to install with 
`i` then type 
`x` to
see El-Get fetch and install all those packages you just selected. Here's a
summary of what's available to you in the 
`M-x el-get-list-packages` buffer:

~~~
Major Mode Bindings:
SPC       	el-get-package-menu-mark-unmark
?         	el-get-package-menu-describe
d         	el-get-package-menu-mark-delete
g         	el-get-package-menu-revert
h         	el-get-package-menu-quick-help
i         	el-get-package-menu-mark-install
u         	el-get-package-menu-mark-update
x         	el-get-package-menu-execute
~~~


Once a package is 
*installed*, El-Get will 
*initialize* it for you, and it will
also do that step at every Emacs startup from there on, provided that you
added some lines to your 
`~/.emacs` initialization file, that look a lot like
the previous 
`*scratch*` code you did paste:

~~~
;;
;; Here's a typical El-Get integration for your .emacs file:
;;
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(setq el-get-user-package-directory "~/.emacs.d/packages.d/")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)
~~~


Then you can add files named like 
`init-<package>.el` in the
`el-get-user-package-directory` directory, those files will get loaded when
El-Get 
*initialize* 
`<package>`.

You can also use 
`M-x el-get-install` if you want to bypass the full screen
package listing, you will get completion on the package name.


## Community and development

El-Get community grew to be a really cool place to be participating in these
days, with core and 
*recipe* contributions from more than 130 different people
already, and with 526 stars on 
`github` and 
`184` forks. I almost can't believe
it!

~~~
git --no-pager shortlog -n -s | wc -l
     137
git --no-pager shortlog -n -s | head -10
   734	Dimitri Fontaine
   336	Ryan C. Thompson
   114	Julien Danjou
   110	Dave Abrahams
    73	Ryan Thompson
    72	S&#xE9;bastien Gross
    42	Takafumi Arakaki
    27	Alex Ott
    25	Yakkala Yagnesh Raghava
    21	R&#xFC;diger Sonderfeld
~~~


Now that we have something that looks like a 
*core team* forming up, I'm
thinking about scheduling much more aggressive stable release. 4.1 has been
very long in the making, I hope to now have a rapid release cycle leading us
to 
`4.2` in quite a short time. As that's not an individual effort by any
mean, though, only history will tell.


## The roadmap

We have lots of ideas and some rough edges to address, so 4.1 is only a stop
in the release history of El-Get. Next ideas include better error management
in face of rare corner cases and in face of external events, like when you
did 
`rm -rf` a directory holding an El-Get managed extension: we should mark
it 
*removed* and clean up the 
`autoloads` that came from it.


## Upgrading to 4.1

This item has received some treatment in the documentation. The basic idea
is that 
`el-get-sources` is no longer what it used to be, it's now only an
alternative source location for 
*recipes*, like it should always have been.
Not that you can still 
*override* in there some properties that you want
*merged* with an official 
*recipe*.

The new thing about 
`el-get-sources` is that it will no longer be the
authoritative list of packages that El-Get manages. That list is not either
given explicitly when calling the 
`el-get` function in your 
`.emacs` setup, or
derived from the packages that are known 
*installed* on your system (like e.g.
`debian` is doing).

Also, given that it took us so much time to brew 
`4.1` a lot of packages have
changed either their hosting location or even switched their 
`SCM`. In such
cases an automatic update of the recipe will no longer be possible, you
might need to 
`el-get-remove` then 
`el-get-install` packages to get them back.


## Conclusion

El-Get 
`4.1` is now ready for public consumption, don't be shy, we've been a
lot of users running the development branch for a long time now, I'm running
`4.0.7.6901194` while writing this post. 
`4.0` is the development version of
what is now released as 
`4.1`.

Many thanks to all who contributed to El-Get and to all our users, I'm very
proud that together we worked out a very nice and complete tool!
