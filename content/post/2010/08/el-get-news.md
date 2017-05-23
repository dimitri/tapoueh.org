+++
date = "2010-08-26T16:30:00.000000+02:00"
title = "el-get news"
tags = ["Emacs", "el-get"]
categories = ["Emacs","el-get"]
thumbnailImage = "/img/old/el-get.big.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/el-get.big.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/08/26-el-get-news",
           "/blog/2010/08/26-el-get-news.html"]
+++

I've been receiving some requests for 
[el-get](http://www.emacswiki.org/emacs/el-get.el), some of them even included a
patch. So now there's support for 
`bzr`, 
`CSV` and 
`http-tar`, augmenting the
existing support for 
`git`, 
`git-svn`, 
`apt-get`, 
`fink` and 
`ELPA` formats.

Also, as the 
`install` and even the 
`build` are completely 
*asynchronous* —
there's a pending bugfix for the building, which is now using
[start-process-shell-command](http://www.gnu.org/software/emacs/elisp/html_node/Asynchronous-Processes.html). The advantage of doing so is that you're free
to use Emacs as usual while 
`el-get` is having your piece of 
`elisp` code
compiled, which can take time.

The drawback is that it's uneasy to to do the associated setup at the right
time without support from 
`el-get`, so you have the new option 
`:after` which
takes a 
`functionp` object: please consider using that to give your own
special setup for the external emacs bits and pieces you're using.

Let's see some examples of the new features:

~~~
(:name xml-rpc-el
	 :type bzr
	 :url "lp:xml-rpc-el")

  (:name haskell-mode
	 :type http-tar
	 :options ("xzf")
	 :url "http://projects.haskell.org/haskellmode-emacs/haskell-mode-2.8.0.tar.gz"
	 :load "haskell-site-file.el"
	 :after (lambda ()
		  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
		  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)))

  (:name auctex
	 :type cvs
	 :module "auctex"
	 :url ":pserver:anonymous@cvs.sv.gnu.org:/sources/auctex"
	 :build ("./autogen.sh" "./configure" "make")
	 :load  ("auctex.el" "preview/preview-latex.el")
	 :info "doc")
~~~


As you can see, there are also the new options 
`:module` (only used by 
`CVS` so
far) and 
`:options` (only used by 
`http-tar` so far). With this later method,
the 
`:options` key allows you to have support for virtually any kind of 
`tar`
compression (
`.tar.bz2`, etc). 

The 
`CVS` support currently does not include authentication against the
anonymous 
`pserver`, because the only repository I've been asked support for
isn't using that, and the couple of servers that I know of are either
wanting no password at the prompt, or a dummy one. That's for another day,
if needed at all.

That pushes the little local hack to more than a thousand lines of 
`elisp`
code, and the next steps include proposing it to 
[ELPA](http://tromey.com/elpa/) so that getting to use
it is easier than ever. You'd just have to choose whether to install 
`ELPA`
from 
`el-get` or the other way around.
