+++
date = "2010-04-06T09:10:00.000000+02:00"
title = "pgloader activity report"
tags = ["PostgreSQL", "release", "prefix", "pgloader"]
categories = ["Projects","pgloader"]
thumbnailImage = "/img/old/toy-loader.320.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/toy-loader.320.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/04/06-pgloader-activity-report",
           "/blog/2010/04/06-pgloader-activity-report.html"]
+++

Yes. This 
[pgloader](http://pgloader.projects.postgresql.org/) project is still maintained and somewhat
active. Development happens when I receive a complaint, either about a bug
in existing code or a feature in yet-to-write code. If you have a bug to
report, just send me an email!

If you're following the development of it, the sources just moved from 
`CVS`
at 
[pgfoundry](http://cvs.pgfoundry.org/cgi-bin/cvsweb.cgi/pgloader/pgloader/) to 
[http://github.com/dimitri/pgloader](http://github.com/dimitri/pgloader). I will still put the
releases at 
[pgfoundry](http://pgfoundry.org/projects/pgloader), and the existing binary packages maintenance should
continue. See also the 
[development version documentation](http://pgloader.projects.postgresql.org/dev/pgloader.1.html), which contains not
yet released stuff.

This time it's about new features, the goal being to open 
*pgloader* usage
without describing all the file format related details into the
`pgloader.conf` file. This time around, 
[Simon](http://database-explorer.blogspot.com/) is giving feedback and told me
he would appreciate that pgloader would work more like the competition.

We're getting there with some new options. The first one is that rather than
only 
`Sections`, now your can give a 
`filename` as an argument. 
*pgloader* will
then create a configuration section for you, considering the file format to
be 
`CSV`, setting 
`columns = *`. The default 
*field separator* is 
`|`,
so you have also the 
`-f, --field-separator` option to set that from the
command line.

As if that wasn't enough, 
*pgloader* now supports any 
[PostgreSQL](http://www.postgresql.org/) option either
in the configuration file (prefix the real name with 
`pg_option_`) or on the
command line, via the 
`-o, --pg-options` switch, that you can use more than
once. Command line setting will take precedence over any other setup, of
course. Consider for example 
`-o standard_conforming_strings=on`.

While at it, some more options can now be set on the command line, including
`-t, --section-threads` and 
`-m, --max-parallel-sections` on the one hand and
`-r, --reject-log` and 
`-j, --reject-data` on the other hand. Those two last
must contain a 
`%s` place holder which will get replaced by the 
*section* name,
or the 
`filename` if you skipped setting up a 
*section* for it.

Your 
*pgloader* usage is now more command line friendly than ever!
