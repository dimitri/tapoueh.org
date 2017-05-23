+++
date = "2009-08-18T09:14:00.000000+02:00"
title = "hstore-new & preprepare reach debian too"
tags = ["debian", "release", "prefix", "preprepare"]
categories = ["Projects","preprepare"]
thumbnailImage = "/img/old/debian-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/debian-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2009/08/18-hstore-new--preprepare-reach-debian-too",
           "/blog/2009/08/18-hstore-new--preprepare-reach-debian-too.html"]
+++

It seems like debian developers are back from annual conference and holiday,
so they have had a look at the 
`NEW` queue and processed the packages in
there. Two of them were mines and waiting to get in 
`unstable`, 
[hstore-new](http://packages.debian.org/hstore-new) and
[preprepare](http://packages.debian.org/preprepare).

Time to do some bug fixing already, as 
`hstore-new` packaging is using a
*bash'ism* I shouldn't rely on (or so the debian buildfarm is 
[telling me](https://buildd.debian.org/~luk/status/package.php?p=hstore-new)) and
for 
`preprepare` I was waiting for inclusion before to go improving the 
`GUC`
management, stealing some code from 
[Selena](http://blog.endpoint.com/search/label/postgres)'s 
[pgGearman](http://blog.endpoint.com/2009/07/pggearman-01-release.html) :)

As some of you wonder about 
`prefix 1.0` scheduling, it should soon get there
now it's been in testing long enough and no bug has been reported. Of course
releasing 
`1.0` in august isn't good timing, so maybe I should just wait some
more weeks.
