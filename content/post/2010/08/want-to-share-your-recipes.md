+++
date = "2010-08-31T14:15:00.000000+02:00"
title = "Want to share your recipes?"
tags = ["Emacs", "Muse", "el-get", "cssh", "rcirc"]
categories = ["Software Programming","Emacs Lisp"]
thumbnailImage = "/img/old/el-get.big.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/el-get.big.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/08/31-want-to-share-your-recipes",
           "/blog/2010/08/31-want-to-share-your-recipes.html"]
+++

Yes, that's another 
[el-get](http://github.com/dimitri/el-get/) related entry. It seems to take a lot of my
attention these days. After having setup the 
`git` repository so that you can
update 
`el-get` from within itself (so that it's 
*self-contained*), the next
logical step is providing 
*recipes*.

By that I mean that 
`el-get-sources` entries will certainly look a lot alike
between a user and another. Let's take the 
`el-get` entry itself:

~~~
(:name el-get
       :type git
       :url "git://github.com/dimitri/el-get.git"
       :features "el-get")
~~~


I guess all 
`el-get` users will have just the same 4 lines in their
`el-get-sources`. So let's call that a 
*recipe*, and have 
`el-get` look for yours
into the 
`el-get-recipe-path` directories. A recipe is found looking in those
directories in order, and must be named 
`package.el`. Now, 
`el-get` already
contains a handful of them, as you can see:

~~~
ELISP> (directory-files "~/dev/emacs/el-get/recipes/" nil "[^.]$")
("auctex.el" "bbdb.el" "cssh.el" "el-get.el" "emms.el" "erc-track-score.el"
 "escreen.el" "google-maps.el" "haskell-mode.el" "hl-sexp.el" "magit.el"
 "muse-blog.el" "nxhtml.el" "psvn.el" "rainbow-mode.el" "rcirc-groups.el"
 "vkill.el" "xcscope.el" "xml-rpc-el.el" "yasnippet.el")
~~~


Please note that you can have your own local recipes by adding directories
to 
`el-get-recipe-path`. So now your minimalistic 
`el-get-sources` list will
look like 
`'(el-get cssh screen)`, say. And if you want to override a recipe,
for instance to use the default one but still have a personal 
`:after`
function containing your own setup, then simply have your 
`el-get-source`
entry a partial entry. Missing 
`:type` and 
`el-get` will merge your local
overrides atop the default one.

Finally, the way to share your recipes is by sending me an email with the
file, or to do the same over the 
`github` interface, I guess I'll still
receive a mail then.
