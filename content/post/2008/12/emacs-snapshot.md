+++
date = "2008-12-08T16:10:00.000000+01:00"
title = "emacs-snapshot"
tags = ["Emacs", "Muse", "debian", "release"]
categories = ["debian"]
thumbnailImage = "/img/old/debian-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/debian-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2008/12/08-emacs-snapshot",
           "/blog/2008/12/08-emacs-snapshot.html"]
+++

If you want to live on the bleeding edge, it's easy enough to get a non
existing release of 
[GNU Emacs](http://www.gnu.org/software/emacs/) under 
[debian sid](http://www.debian.org/releases/unstable/), thanks to
[http://emacs.orebokech.com/](http://emacs.orebokech.com/).

The problem is that 
[Emacs Muse](http://mwolson.org/projects/EmacsMuse.html) is broken on 
`emacs-snapshot`, partly because
of 
[Htmlize](http://www.emacswiki.org/emacs/Htmlize) which is unable to find the face fonts (I got 
`(error "Invalid
face")`), partly because of my configuration itself:

~~~
hunk ./dim-muse.el 22
-      '(("pgsql.tapoueh.org" $
-        (,@(muse-project-alist-dirs "~/dev/muse/site") $
+      '(("pgsql.tapoueh.org" ("~/dev/muse/site"
+        ;;(,@(muse-project-alist-dirs "~/dev/muse/site") $
~~~


The solution was to switch to using 
`Emacs 22` on sid for 
[pgsql.tapoueh.org](http://pgsql.tapoueh.org/site/muse/site/)
editing, while using 
[EmacsCVS](http://www.emacswiki.org/emacs/?action=browse;oldid=EmacsCVS;id=EmacsFromCVS) for other activities.

And I'm using the patched 
`Htmlize` on both the versions, by the way.
