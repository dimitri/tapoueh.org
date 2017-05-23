+++
date = "2011-08-02T10:30:00.000000+02:00"
title = "See Tsung in action"
tags = ["PostgreSQL", "Tsung"]
categories = ["PostgreSQL"]
thumbnailImage = "/img/postgresql-512.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/postgresql-512.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/08/02-see-tsung-in-action",
           "/blog/2011/08/02-see-tsung-in-action.html"]
+++

[Tsung](http://tsung.erlang-projects.org/) is an open-source multi-protocol distributed load testing tool and a
mature project.  It's been available for about 10 years and is built with
the 
[Erlang](http://www.erlang.org/) system.  It supports several protocols, including the 
[PostgreSQL](http://www.postgresql.org/)
one.

When you want to benchmark your own application, to know how many more
clients it can handle or how much gain you will see with some new shiny
hardware, 
[Tsung](http://tsung.erlang-projects.org/) is the tool to use.  It will allow you to 
*record* a number of
sessions then replay them at high scale.  
[pgfouine](http://pgfouine.projects.postgresql.org/tsung.html) supports Tsung and is
able to turn your PostgreSQL logs into Tsung sessions, too.

Tsung did get used in the video game world, their version of it is called
[uTsung](http://www.developer.unitypark3d.com/tools/utsung/), apparently using the 
[uLink](http://www.developer.unitypark3d.com/index.html) game development facilities.  They even
made a video demo of uTsung, that you might find interresting:

    

<div class="figure center dim-margin">
  <a href="http://www.youtube.com/watch?v=rxBhqIP_7ls">
    <img src="/img/old/utsung-demo.png">
  </a>
</div>

