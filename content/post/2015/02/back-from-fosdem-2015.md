+++
date = "2015-02-09T10:36:00.000000+01:00"
title = "Back From FOSDEM 2015"
tags = ["PostgreSQL", "Conferences", "FOSDEM", "pgloader"]
categories = ["Conferences","PostgreSQL Confs"]
thumbnailImage = "/img/old/Fosdem-2015-Tizen-Developer2.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/Fosdem-2015-Tizen-Developer2.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2015/02/09-back-from-fosdem-2015",
           "/blog/2015/02/09-back-from-fosdem-2015.html"]
+++

The 
[FOSDEM 2015](https://fosdem.org/2015/) edition has been awesome this year, the usual mix of meeting
with old friends, talking about interesting topics, seeing tremendous
activity in all Open Source domains, and having Belgium beers in the
evenings.


<div class="figure center dim-margin">
  <a href="https://fosdem.org/2015/">
    <img src="/img/old/Fosdem-2015-Tizen-Developer2.jpg">
  </a>
</div>


## FOSDEM PGDAY

On the Friday before the real FOSDEM event our own 
[PostgreSQL Europe](https://www.postgresql.eu/)
organized a one-day event, the 
[FOSDEM PGDAY](http://fosdem2015.pgconf.eu/). It as an intense day of
conferences about PostgreSQL, where I had the opportunity to present
[pgloader](http://pgloader.io/) in the context of dealing with database migrations.


<div class="figure center dim-margin">
  <a href="/images/confs/Fosdem_2015_pgloader.pdf">
    <img src="/img/old/Fosdem_2015_pgloader.png">
  </a>
</div>

*Migrate from MySQL to PostgreSQL in one command*

## PostgreSQL User Group, Paris Meetup

This presentation about migrating to PostgreSQL was also given at the
[PostgreSQL User Group Meetup in Paris](http://www.meetup.com/PostgreSQL-User-Group-Paris/events/220230052/) more recently, and I'm happy to
announce here that we have more than 200 registered members in the group
now!

Check out our 
[next meetup](http://www.meetup.com/PostgreSQL-User-Group-Paris/events/220351563/) which is already scheduled!


## FOSDEM

At the FOSDEM event proper I had the pleasure to present my recent talk
about backups:


<div class="figure center dim-margin">
  <a href="/images/confs/Fosdem_2015_backups.pdf">
    <img src="/img/old/Fosdem_2015_backups.png">
  </a>
</div>

*Nobody cares about backups, think about data recovery*

If you want to remember only one thing about that presentation, it must be
that we don't care about how you take backups, we only care about if you're
able to recover data in worst case scenarios. The only to check a backup is
to recover it. Do automated testing of your backups, which means automated
recovery.
