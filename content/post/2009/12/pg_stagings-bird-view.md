+++
date = "2009-12-08T12:04:00.000000+01:00"
title = "pg_staging's bird view"
tags = ["Emacs", "pg_staging"]
categories = ["Projects","pg_staging"]
thumbnailImage = "/img/old/pg_staging.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/pg_staging.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2009/12/08-pg_stagings-bird-view",
           "/blog/2009/12/08-pg_stagings-bird-view.html"]
+++

One of the most important feedback I got about the presentation of 
[pgstaging](pgstaging.html)
were the lack of pictures, something like a bird-view of how you operate
it. Well, thanks to 
[ditaa](http://ditaa.sourceforge.net/) and Emacs 
`picture-mode` here it is:

Hope you enjoy, it should not be necessary to comment much if I got to the
point!

Of course I commited the 
[text source file](http://github.com/dimitri/pg_staging/blob/master/bird-view.txt) to the 
`Git` repository. The only
problem I ran into is that 
`ditaa` defaults to ouputing a quite big right
margin containing only white pixels, and that didn't fit well, visually, in
this blog. So I had to resort to 
[ImageMagik crop command](http://www.imagemagick.org/script/command-line-options.php#crop) in order to avoid
any mouse usage in the production of this diagram.

~~~
convert .../pg_staging/bird-view.png -crop '!550' bird-view.png
mv bird-view-0.png pg_staging.png
~~~


Quicker than learning to properly use a mouse, at least for me :)
