+++
date = "2011-02-01T13:35:00.000000+01:00"
title = "Going to FOSDEM"
tags = ["PostgreSQL", "Conferences", "Extensions", "FOSDEM", "9.1"]
categories = ["Conferences","PostgreSQL Confs"]
thumbnailImage = "/img/old/going-to-fosdem-2011.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/going-to-fosdem-2011.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/02/01-going-to-fosdem",
           "/blog/2011/02/01-going-to-fosdem.html"]
+++

A quick blog entry to say that yes:

And I will even do my 
[Extension's talk](http://fosdem.org/2011/schedule/event/pg_extension1) which had a 
[success at pgday.eu](http://blog.hagander.net/archives/183-Feedback-from-PGDay.EU-the-speakers.html).  The
talk will be updated to include the last developments of the extension's
feature, as some of it changed already in between, and to detail the plan
for the 
`ALTER EXTENSION ... UPGRADE` feature that I'd like to see included as
soon as 
`9.1`, but time is running so fast.

In fact the design for the 
`UPGRADE` has been done and reviewed already, but
there's yet to reach consensus on how to setup which is the upgrade file to
use when upgrading from a given version to another.  I've solved it in my
patch, of course, by adding properties into the extension's 
*control
file*. That's the best place to have that setup I think, it allows lots of
flexibility, leave the extension's author in charge, and avoids any hard
coding of any kind of assumptions about file naming or whatever.

Next days and reviews will tell us more about how the design is received.
Meanwhile, we're working on finalizing the main extension's patch, offering
`pg_dump` support.

See you at 
[FOSDEM](http://fosdem.org/2011/)!
