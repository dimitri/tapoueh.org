+++
title = "PostgreSQL Data Types: Network Addresses"
date = "2018-04-16T12:32:53+02:00"
tags = ["PostgreSQL","YeSQL","Data Types","Network Address","CIDR","INET"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/optic-fibre.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/Network-Ip-Address-icon.png"
thumbnailImagePosition = "left"

+++

Continuing our series of [PostgreSQL Data Types](/tags/data-types/) today
we're going to introduce network address types.

PostgreSQL includes support for both *cidr*, *inet*, and *macaddr* data
types. Again, those types are bundled with indexing support and advanced
functions and operator support.

<!--more-->
<!--toc-->

# Network Address Types

The PostgreSQL documentation chapters entitled [Network Address
Types](https://www.postgresql.org/docs/current/static/datatype-net-types.html)
and [Network Address Functions and
Operators](https://www.postgresql.org/docs/current/static/functions-net.html)
cover network address types.

Web servers logs are a classic source of data to process where we find
network address types and [The Honeynet
Project](http://old.honeynet.org/scans/scan34/) has some free samples for us
to play with. This time we're using the *Scan 34* entry. Here's how to load
the sample data set, once cleaned into a proper CSV file:

~~~ sql
begin;

drop table if exists access_log;

create table access_log
 (
  ip      inet,
  ts      timestamptz,
  request text,
  status  integer
 );

\copy access_log from 'access.csv' with csv delimiter ';'

commit;
~~~

The script used to cleanse the original data into a CSV that PostgreSQL is
happy about implements a pretty simple transformation from

~~~
211.141.115.145 - - [13/Mar/2005:04:10:18 -0500] "GET / HTTP/1.1" 403 2898 "-" "Mozilla/4.0 (compatible; MSIE 5.5; Windows 98)"
~~~

into 

~~~ csv
"211.141.115.145";"2005-05-13 04:10:18 -0500";"GET / HTTP/1.1";"403"
~~~

Being mostly interested into network address types, the transformation from
the Apache access log format to CSV is lossy here, we keep only some of the
fields we might be interested into.

One of the things that's possible to implement thanks to the PostgreSQL
*inet* data type is an analysis of */24* networks that are to be found in
the logs.

# Network Address Masks, CIDR

To enable that analysis, we can use the *set_masklen()* function which
allows us to transforms an IP address into an arbitrary CIDR network
address:

~~~ sql
select distinct on (ip)
       ip,
       set_masklen(ip, 24) as inet_24,
       set_masklen(ip::cidr, 24) as cidr_24
  from access_log
 limit 10;
~~~

And we can see that if we keep the data type as *inet*, we still get the
full IP address with the */24* network notation added. To have the *.0/24*
notation we need to be using *cidr*:

~~~ pqsl
      ip       │     inet_24      │     cidr_24     
═══════════════╪══════════════════╪═════════════════
 4.35.221.243  │ 4.35.221.243/24  │ 4.35.221.0/24
 4.152.207.126 │ 4.152.207.126/24 │ 4.152.207.0/24
 4.152.207.238 │ 4.152.207.238/24 │ 4.152.207.0/24
 4.249.111.162 │ 4.249.111.162/24 │ 4.249.111.0/24
 12.1.223.132  │ 12.1.223.132/24  │ 12.1.223.0/24
 12.8.192.60   │ 12.8.192.60/24   │ 12.8.192.0/24
 12.33.114.7   │ 12.33.114.7/24   │ 12.33.114.0/24
 12.47.120.130 │ 12.47.120.130/24 │ 12.47.120.0/24
 12.172.137.4  │ 12.172.137.4/24  │ 12.172.137.0/24
 18.194.1.122  │ 18.194.1.122/24  │ 18.194.1.0/24
(10 rows)
~~~

Of course, note that you could be analyzing other networks than */24*:

~~~ sql
select distinct on (ip)
       ip,
       set_masklen(ip::cidr, 27) as cidr_27,
       set_masklen(ip::cidr, 28) as cidr_28
  from access_log
 limit 10;
~~~

This computes for us the proper starting ip addresses for our CIDR notation
for us, of course. After all, what's the point of using proper data types if
not for advanced processing?

~~~ psql
      ip       │     cidr_27      │     cidr_28      
═══════════════╪══════════════════╪══════════════════
 4.35.221.243  │ 4.35.221.224/27  │ 4.35.221.240/28
 4.152.207.126 │ 4.152.207.96/27  │ 4.152.207.112/28
 4.152.207.238 │ 4.152.207.224/27 │ 4.152.207.224/28
 4.249.111.162 │ 4.249.111.160/27 │ 4.249.111.160/28
 12.1.223.132  │ 12.1.223.128/27  │ 12.1.223.128/28
 12.8.192.60   │ 12.8.192.32/27   │ 12.8.192.48/28
 12.33.114.7   │ 12.33.114.0/27   │ 12.33.114.0/28
 12.47.120.130 │ 12.47.120.128/27 │ 12.47.120.128/28
 12.172.137.4  │ 12.172.137.0/27  │ 12.172.137.0/28
 18.194.1.122  │ 18.194.1.96/27   │ 18.194.1.112/28
(10 rows)
~~~

# Network Mask Based Reporting

Equipped with this *set_masklen()* function, it's now easy to analyze our
access logs using arbitrary CIDR network definitions.

~~~ sql
  select set_masklen(ip::cidr, 24) as network,
         count(*) as requests,
         array_length(array_agg(distinct ip), 1) as ipcount
    from access_log
group by network
  having array_length(array_agg(distinct ip), 1) > 1
order by requests desc, ipcount desc;
~~~

In our case, we get the following result:

~~~ psql
     network      │ requests │ ipcount 
══════════════════╪══════════╪═════════
 4.152.207.0/24   │      140 │       2
 222.95.35.0/24   │       59 │       2
 211.59.0.0/24    │       32 │       2
 61.10.7.0/24     │       25 │      25
 222.166.160.0/24 │       25 │      24
 219.153.10.0/24  │        7 │       3
 218.78.209.0/24  │        6 │       4
 193.109.122.0/24 │        5 │       5
 204.102.106.0/24 │        3 │       3
 66.134.74.0/24   │        2 │       2
 219.133.137.0/24 │        2 │       2
 61.180.25.0/24   │        2 │       2
(12 rows)
~~~

# Conclusion

When analyzing logs containing IP addresses in ipv4 and ipv6 formats is
something you need to do, then PostgreSQL has you covered here with its CIDR
and INET datatypes. Not only will PostgreSQL make sure that the values
managed actually are IP addresses, it knows how to do basic processing of
the network addresses.

{{< figure class="right"
             src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
            link="https://masteringpostgresql.com" >}}

This article is an extract from my book [Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com), which teaches SQL to
developers so that they may replace thousands of lines of code with very
simple queries. The book has a full chapter about data types in PostgreSQL,
check it out!
