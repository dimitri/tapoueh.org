+++
date = "2013-02-08T15:52:00.000000+01:00"
title = "Live Upgrading PGQ"
tags = ["PostgreSQL", "Skytools", "PGQ", "debian"]
categories = ["debian"]
thumbnailImage = "/img/old/software-upgrade.320.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/software-upgrade.320.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/02/08-PGQ-Live-Upgrade",
           "/blog/2013/02/08-PGQ-Live-Upgrade.html"]
+++

Some 
[skytools](http://skytools.projects.pgfoundry.org/skytools-3.0/doc/) related new today, it's been a while. For those who where at
my 
[FOSDEM's talk](http://tapoueh.org/blog/2013/02/04-Another-great-FOSDEM.html) about 
[Implementing High Availability](https://fosdem.org/2013/schedule/event/postgresql_implementing_high_availability/) you might have heard
that I really like working with 
[PGQ](http://wiki.postgresql.org/wiki/Skytools#PgQ). A new version has been released a while
ago, and the most recent verion is now 
`3.1.3`, as announced in the
[Skytools 3.1.3](http://www.postgresql.org/message-id/CACMqXCLD2je5VFqUCzjwC2s5QQVYLe6-4awJaRvqLSBEVw8_MQ@mail.gmail.com) email.


*Upgrade time!*

## Skytools 3.1.3 enters debian

First news is that 
*Skytools 3.1.3* has been entering 
[debian](http://packages.debian.org/search?keywords=skytools3) today (I hope
that by the time you reach that URL, it's been updated to show information
according to the news here, but I might be early). As there's current a
*debian freeze* to release 
*wheezy* (and you can help 
[squash some bugs](http://www.debian.org/News/2012/20121110)), this
version is only getting uploaded to 
*experimental* for now. Thanks to the
tireless work of 
[Christoph Berg](http://www.df7cb.de/blog/2012/apt.postgresql.org.html) though, this version is already available
from 
[apt.postgresql.org](https://wiki.postgresql.org/wiki/Apt).


## Upgrading to PGQ 3

The other news is that I've been testing 
*live upgrade* scenario where we want
to upgrade from 
`PGQ` to 
`PGQ3`, and it works pretty well, and it's quite simple
to achieve too. Here's how.

So the first thing is to shut down the current 
*ticker* process. Then we
install the new packages, assuming that you did follow the step in the wiki
pointed above, please go read 
[apt.postgresql.org](https://wiki.postgresql.org/wiki/Apt) again now if needs be.

~~~
pgqadm.py ticker.ini -s
sudo apt-get install postgresql-9.1-pgq3 skytools3-ticker skytools3
~~~


The ticker is not running anymore, we have the right version of the software
installed. Next step is to upgrade the database parts of PGQ:

~~~
psql -f /usr/share/skytools3/pgq.upgrade_2.1_to_3.0.sql ...
psql -1 -f /usr/share/postgresql/9.1/contrib/pgq.upgrade.sql ...
~~~


Of course replace those 
`...` with options such as your actual connection
string. I tend to always add 
`-vON_ERROR_STOP=1` to all these
commands, so that I don't depend on having the right 
`.psqlrc` on the
particular server I'm connected to. Also remember that if you want to do
that for more than one database, you need to actually run that pair of
commands for each of them.

Now it's time to restart the new ticker. The main changes from the previous
one is that it is now a 
`C` program called 
`pgqd` that knows how to tick for any
number of 
*databases*, so that you only have to have 
*one instance* around 
*per
cluster* now.

~~~
sudo /etc/init.d/skytools3 start
tail -f /var/log/skytools/pgqd.log
~~~


Those two commands are taking for granted that you did prepare the 
`pgqd`
setup the 
*debian* and 
*skytools* way, by adding your config in
`/etc/skytools3/pgqd.ini` and editing 
`/etc/skytools.ini` accordingly, so that
it's automatically taken into account at machine boot.

Note that I did actually exercised the procedure above while running a
[pgbench](http://www.postgresql.org/docs/9.2/static/pgbench.html) test replicated with 
`londiste`. Of course the replication has been
lagging a little while no 
*ticker* was running, and then it catched-up as fast
as it could, in that case:

~~~
INFO {count: 245673, ignored: 0, duration: 422.104366064}
~~~



## Happy Hacking!

So if you have any 
*batch processing* needs, remember to consider what PGQ has
to offer. And yes if you're running some cron job to compute things out of
the database for you, you are doing some 
*batch processing*.


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/hayseed.jpg" >}}


*Yes, I did search for Transactional Batch Processing*