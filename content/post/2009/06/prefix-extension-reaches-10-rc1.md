+++
date = "2009-06-23T10:53:00.000000+02:00"
title = "prefix extension reaches 1.0 (rc1)"
tags = ["PostgreSQL", "debian", "release", "prefix"]
categories = ["Projects","prefix"]
thumbnailImage = "/img/old/debian-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/debian-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2009/06/23-prefix-extension-reaches-10-rc1",
           "/blog/2009/06/23-prefix-extension-reaches-10-rc1.html"]
+++

At long last, after millions and millions of queries just here at work and
some more in other places, the 
[prefix](prefix.html) project is reaching 
`1.0` milestone. The
release candidate is getting uploaded into debian at the moment of this
writing, and available at the following place: 
[prefix-1.0~rc1.tar.gz](http://prefix.projects.postgresql.org/prefix-1.0~rc1.tar.gz).

If you have any use for it (as some 
*VoIP* companies have already), please
consider testing it, in order for me to release a shiny 
`1.0` next week! :)

Recent changes include getting rid of those square brackets output when it's
not neccesary, fixing btree operators, adding support for more operators in
the 
`GiST` support code (now supported: 
`@>`, 
`<@`, 
`=`, 
`&&`). Enjoy!
