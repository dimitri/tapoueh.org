+++
date = "2014-08-25T14:09:00.000000+02:00"
title = "Turn your PostgreSQL queries into Charts"
tags = ["PostgreSQL", "Common-Lisp", "MongoDB"]
categories = ["Software Programming","Common Lisp"]
thumbnailImage = "/img/old/pgcharts-chart.640.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/pgcharts-chart.640.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2014/08/25-pgcharts",
           "/blog/2014/08/25-pgcharts.html"]
+++

Earlier this year we did compare compare
[Aggregating NBA data, PostgreSQL vs MongoDB](/blog/2014/02/17-aggregating-nba-data-PostgreSQL-vs-MongoDB) then talked about
[PostgreSQL, Aggregates and histograms](/blog/2014/02/21-PostgreSQL-histogram) where we even produced a nice
*Histogram* chart directly within the awesome 
`psql` console. Today, let's get
that same idea to the next level, with 
[pgcharts](https://github.com/dimitri/pgcharts):


<div class="figure center dim-margin">
  <a href="https://github.com/dimitri/pgcharts">
    <img src="/img/old/pgcharts-chart.640.png">
  </a>
</div>

*The new [pgcharts](https://github.com/dimitri/pgcharts) application*

The application's specifications are quite simple: edit an SQL query, set
your 
*categories* and your 
*data series*, add in some 
*legends*, and get a nice
chart. Currently supported are 
*bar*, 
*column*, 
*pie* and 
*donut* charts, and we
should be able to add anything that 
[http://www.highcharts.com/](Highcharts) has support
for.


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/pgcharts-query.640.png" >}}


Currently, you need to compile the application yourself, and for that you
need to install the 
[SBCL](http://sbcl.org/platform-table.html) compiler. Soon enough you will have a 
*debian
package* to play with! The 
[README](https://github.com/dimitri/pgcharts/blob/master/README.md) at the 
[pgcharts github place](https://github.com/dimitri/pgcharts) has the
details to get you started. Enjoy!
