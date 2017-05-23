+++
date = "2010-08-04T22:30:00.000000+02:00"
title = "el-get"
tags = ["Emacs", "Muse", "debian", "el-get"]
categories = ["debian"]
thumbnailImage = "/img/old/debian-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/debian-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/08/04-el-get",
           "/blog/2010/08/04-el-get.html"]
+++

I've been using emacs for a long time, and a long time it took me to
consider learning 
[Emacs Lisp](http://www.gnu.org/software/emacs/emacs-lisp-intro/html_node/index.html). Before that, I didn't trust my level of
understanding enough to be comfortable in managing my setup efficiently.

One of the main problems of setting up 
[Emacs](http://www.gnu.org/software/emacs/) is that not only you tend to
accumulate so many tricks from 
[EmacsWiki](http://www.emacswiki.org/) and 
[blog posts](http://planet.emacsen.org/) that your 
`.emacs` has
to grow to a full 
`~/.emacs.d/` directory (starting at 
`~/.emacs.d/init.el`),
but also you finally depend on several 
*librairies* of code you're not
authoring nor maintaining. Let's call them 
*packages*.

Some of them will typically be available on 
[ELPA](http://tromey.com/elpa/index.html), which allows you to
breathe and keep cool. But most of them, let's face it, are not there. Most
of the packages I use I tend to get them either from 
[debian](http://www.debian.org/) (see
[apt-rdepends](http://packages.debian.org/sid/apt-rdepends) for having the complete list of packages that depends on emacs,
unfortunately I'm not finding an online version of the tool to link too), or
from 
`ELPA`, or from their own 
`git` repository somewhere. Some of them even I
get directly from an 
[obscure website](http://www.splode.com/~friedman/software/emacs-lisp) not maintained anymore, but always
there when you need them.

Of course, my emacs setup is managed in a private 
`git` repository. Some
people on 
`#emacs` are using 
[git submodules](http://www.kernel.org/pub/software/scm/git/docs/git-submodule.html) (or was it straight 
*import*) for
managing external repositories in there, but all I can say is that I frown
on this idea. I want an easy canonical list of packages I depend on to run
emacs, and I want this documentation to be usable as-is. Enters 
[el-get](http://www.emacswiki.org/emacs/el-get.el)!

As we're all damn lazy, here's a 
*visual* introduction to 
`el-get`:

~~~
(setq el-get-sources
      '((:name bbdb
	       :type git
	       :url "git://github.com/barak/BBDB.git"
	       :load-path ("./lisp" "./bits")
	       :info "texinfo"
	       :build ("./configure" "make"))
	
	(:name magit
	       :type git
	       :url "http://github.com/philjackson/magit.git"
	       :info "."
	       :build ("./autogen.sh" "./configure" "make"))
	
	(:name vkill
	       :type http
	       :url "http://www.splode.com/~friedman/software/emacs-lisp/src/vkill.el"
	       :features vkill)
	
	(:name yasnippet
	       :type git-svn
	       :url "http://yasnippet.googlecode.com/svn/trunk/")
	
	(:name asciidoc         :type elpa)
	(:name dictionary-el    :type apt-get)
	(:name emacs-goodies-el :type apt-get)))

(el-get)
~~~


So now you have a pretty good documentation of the packages you want
installed, where to get them, and how to install them. For the 
*advanced*
methods (such as 
`elpa` or 
`apt-get`), you basically just need the package
name. When relying on a bare 
`git` repository, you need to give some more
information, such as the 
`URL` to 
*clone* and the 
`build` steps if any. Then also
what 
*features* to 
`require` and maybe where to find the 
*texinfo* documentation
of the package, for automatic inclusion into your local 
*Info* menu.

The good news is that not only you now have a solid readable description of
all that in a central place, but this very description is all 
`(el-get)` needs
to do its magic. This command will check that each and every package is
installed on your system (in 
`el-get-dir`) and if that's not the case, it will
actually install it. Then, it will 
`init` the packages: that means caring
about the 
`load-path`, the 
`Info-directory-list` (and 
*dir* texinfo menu
building), the 
*loading* of the 
`emacs-lisp` files, and finally it will 
`require`
the 
*features*.

Here's a prettyfied 
`ielm` session that will serve as a demo:

~~~
ELISP> (el-get)
("aspell-en" "aspell-fr" "muse" "dictionary" "htmlize" "bbdb" "google-maps"
"magit" "emms" "nxhtml" "vkill" "xcscope" "yasnippet" "asciidoc"
"auto-dictionary" "css-mode" "gist" "lua-mode" "lisppaste") 
~~~


All the packages being already installed, it's running fast enough that I
won't bother measuring the run time, that seems to be somewhere around one
second.
