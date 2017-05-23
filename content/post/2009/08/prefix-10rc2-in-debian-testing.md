+++
date = "2009-08-03T14:50:00.000000+02:00"
title = "prefix 1.0~rc2 in debian testing"
tags = ["debian", "release", "prefix", "preprepare"]
categories = ["Projects","preprepare"]
thumbnailImage = "/img/old/debian-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/debian-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2009/08/03-prefix-10rc2-in-debian-testing",
           "/blog/2009/08/03-prefix-10rc2-in-debian-testing.html"]
+++

At long last, 
[here it is](http://packages.debian.org/search?searchon=sourcenames&keywords=prefix). With binary versions both for 
`postgresal-8.3` and
`postgresal-8.4`! Unfortunately my other packaging efforts are still waiting
on the 
`NEW` queue, but I hope to soon see 
`hstore-new` and 
`preprepare` enter
debian too.

Anyway, the plan for 
`prefix` is to now wait something like 2 weeks, then,
baring showstopper bugs, release the 
`1.0` final version. If you have a use
for it, now is the good time for testing it!

About upgrading a current 
`prefix` installation, the advice is to save data as
`text` instead of 
`prefix_range`, remove prefix support, install new version,
change again the columns data type:

~~~
BEGIN;
  ALTER TABLE foo
     ALTER COLUMN prefix
             TYPE text USING text(prefix);

  DROP TYPE prefix_range CASCADE;
  \i prefix.sql

  ALTER TABLE foo
     ALTER COLUMN prefix
             TYPE prefix_range USING prefix_range(prefix);

  CREATE INDEX idx_foo_prefix ON foo
         USING gist(prefix gist_prefix_range_ops);
COMMIT;
~~~


Note: I just added the 
`gist_prefix_range_ops` as default for type
`prefix_range` so it'll be optional to specify this in final 
`1.0`. I got so
used to typing it I didn't realize we don't have to :)
