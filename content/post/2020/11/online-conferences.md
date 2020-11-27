+++
title = "2020: Online Conferences"
date = "2020-11-27T13:10:00+01:00"
tags = ["PostgreSQL","YeSQL","Conferences","Failover","pg_auto_failover"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/online-conferences.jpg"
coverMeta = "out"
coverSize = "partial"
thumbnailImage = "/img/webcam-svgrepo-com.svg"
thumbnailImagePosition = "left"

+++

Among a lot of other changes, the year 2020 brings Online Conferences to us.
In the Postgres community too we now record our talks at home and send a
video file to be playedto a virtual audience, and sometimes shared later in
a platform online.

So this year I did participate in [Postgres Vision
2020](https://www.postgresvision.com/) where I did deliver a talk about [The
Art of PostgreSQL](https://sched.co/cTyt). This a talk all about the book
that I have written and self-publish at [The Art of
PostgreSQL](https://theartofpostgresql.com): learn how to turn thousands of
lines of code into simple SQL queries.

<hr />

Earlier this year I also had the honour to be invited to the
<http://Postgres.TV> channel for a very interesting online-meetup concept by
[Nikolay Samokhvalov](https://twitter.com/samokhvalov) and [Ilya
Kosmodemiansky](https://twitter.com/the_hydrobiont). They did prepare the
interview well and this online meetup felt to me like a warm conversation
with friends, which it was, in its own 2020 style (at a distance, everyone
staying safe at home). The interview is now available online there:

{{< youtube Ztvst8IxtjE >}}

In this interview on Postgres.TV we talked about several topics, and I feel
like I should dive more into several of them.

  - First, Nikolay observed that this blog website of mine
    ([tapoueh.org](https://tapoueh.org)) did not see an update in a year
    now... so this very article is me getting back on track to publishing
    again on the blog
    
  - Then, the first part of the interview focused on several of the key
    ideas that I dive in in my book, including [SQL as code to implement
    your Business
    Logic](https://tapoueh.org/blog/2017/06/sql-and-business-logic/).
    
    There are more than one way to approach that idea. The one that I like
    the most so far is to say that as soon as you write (or have an ORM
    generate) a SQL query you're implementing a part of the business logic
    in there already. Sometimes it's just a tiny part: the fact that the
    query is SELECTing this column from this table is already embedding
    some of the business logic, after all.
    
    So the question that is interesting to me is about how to split your
    business logic implementation between the SQL side of things, and the
    application SQL-client side of things. Note that usually the part of tha
    application code that is the client for SQL queries is the backend.
    
    I think this idea is worth in-depth study and I am planning on writing a
    series of blog posts about this very topic. That might end-up in a
    dedicated eBook on the topic, or as a part of my next book. There should
    be a second volume to [The Art of
    PostgreSQL](https://theartofpostgresql.com), and now I have mentionned
    it publicly!
    
  - Finally, we spent a good chunk of time talking about the
    [pg_auto_failover](https://github.com/citusdata/pg_auto_failover)
    project, an Open Source solution for implementing Postgres High
    Availability. That's the project that has kept me busy in 2019 and 2020
    and probably will keep me busy in the 2021 too.
    
<hr />

Next up in my online virtual conference circuit of 2020 is [Postgres
Build](https://www.postgresbuild.com/) where I'm going to talk about
[Postgres Architectures in Production](https://sched.co/fzfB).

In this talk that I recorded already (the video still needs to be processed,
captions added, etc), I take some time to dive in what Postgres includes
out-of-the-box to implement HA, and what's missing in Core Postgres.

The idea behind
[pg_auto_failover](https://github.com/citusdata/pg_auto_failover) is to fill
the gap in our approach to HA in the Postgres community, by using all of the
solid building blocks that Postgres is offering and actually building a
simple and robust solution out of those.

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/arch-three-standby-one-async.svg" >}}
</center>

Have a look at the [pg_auto_failover project
documentation](https://pg-auto-failover.readthedocs.io/en/latest/intro.html)
online and give it a try. If you fancy building from source, and if you are
a tmux user, try the following recipe:

~~~
$ git clone https://github.com/citusdata/pg_auto_failover.git
$ cd pg_auto_failover
$ make -s clean install
$ make NODES=3 cluster
~~~

That's going to prepare a 3-nodes cluster for you, with a primary and two
secondary nodes. Also you're going to have a nice tmux integration for
interactivly controling the cluster:

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/pg_auto_failover_cluster.png" >}}
</center>

Now and with a free shell area where you can orchestrate your first
`switchover` using:

~~~
$ pg_autoctl perform switchover
~~~

And if you want to see what the next image would look like, well I guess you
have to try it yourself now.

When you are using the binary packages distribution rather than building
from sources, you may still enjoy the same facility with the more involved
command:

~~~
$ PG_AUTOCTL_DEBUG=1 pg_autoctl do tmux session        \
           --root ./tmux                               \
           --first-pgport 5500                         \
           --nodes 3                                   \
           --layout even-vertical	
~~~

Now, let's get back to 2020, online conferences, remote activities with a
strong focus of staying safe both physically and mentally... so finding ways
to still have fun!
