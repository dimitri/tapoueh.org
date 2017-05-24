+++
date = "2011-08-26T21:30:00.000000+02:00"
title = "Skytools, version 3"
tags = ["PostgreSQL", "Skytools"]
categories = ["PostgreSQL","Skytools"]
thumbnailImage = "/img/old/londiste_logo.gif"
thumbnailImagePosition = "left"
coverImage = "/img/old/londiste_logo.gif"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/08/26-skytools3",
           "/blog/2011/08/26-skytools3.html"]
+++

You can find 
[skytools3](http://packages.debian.org/source/experimental/skytools3) in debian experimental already, it's in 
*release
candidate* status.  What's missing is the documentation, so here's an idea:
I'm going to make a blog post series about 
[skytools](https://github.com/markokr/skytools) next features, how to
use them, what they are good for, etc.  This first article of the series
will just list what are those new features.

Here are the slides from the 
[CHAR(11)](http://www.char11.org/) talk I made last month, about that
very subject:

<center>
<div class="figure dim-margin">
  <a href="/images/confs/CHAR_2011_Skytools3.pdf">
    <img src="/img/old/CHAR_2011_Skytools3.png">
  </a>
</div>
</center>

The new version comes with a lot of new features.  
`PGQ` now is able to
duplicate the queue events from one node to the next, so that it's able to
manage 
*switching over*.  To do that we have three types of nodes now, 
*root*,
*branch* and 
*leaf*.  
`PGQ` also supports 
*cooperative consumers*, meaning that you
can share the processing load among many 
*consumers*, or workers.

`Londiste` now benefits from the 
*switch over* feature, and is packed with new
little features like 
`add <table> --create`, the new 
`--trigger-flags` argument,
and the new 
`--handler` thing (to do e.g. partial table replication).  Let's
not forget the much awaited 
`execute <script>` command that allows to include
`DDL` commands into the replication stream, nor the 
*parallel* 
`COPY` support that
will boost your initial setup.

`walmgr` in the new version behaves correctly when using 
[PostgreSQL](http://www.postgresql.org) 9.0.
Meaning that as soon as no more 
*WAL* files are available in the archives, it
returns an error code to the 
*archiver* so that the server switches to
*streaming* live from the 
`primary_conninfo`, then back to replaying the files
from the archive if the connection were to fail, etc.  All in all, it just
works.

Details to follow here, stay tuned!
