+++
date = "2013-08-08T17:41:00.000000+02:00"
title = "Migrating from MySQL to PostgreSQL"
tags = ["PostgreSQL", "MySQL", "Migration", "pgloader"]
categories = ["Projects","pgloader"]
thumbnailImage = "/img/old/postgresql_versus_mysql.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/postgresql_versus_mysql.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/08/08-MySQL-to-PostgreSQL",
           "/blog/2013/08/08-MySQL-to-PostgreSQL.html"]
+++

About the only time when I will accept to work with MySQL is when you need
help to migrate away from it because you decided to move to 
[PostgreSQL](http://www.postgresql.org/)
instead. And it's already been too much of a pain really, so after all this
time I began consolidating what I know about that topic and am writing a
software to help me here. Consider it the 
***MySQL Migration Toolkit***.


*A real classic that I couldn't resist using here...*

Without further ado, here's a little demo of the test case I have here to
work on the problem at hand. I've been working from a new version of
[pgloader](/pgsql/pgloader.html) and added to it, among other novelties and a complete new
implementation, a 
*command language*. Here's what the 
**MySQL Migration to
PostgreSQL command** looks like currently:

~~~
load database from mysql://localhost/adv
              into postgresql://dim@localhost/adv

with drop tables, truncate, create tables, create indexes, reset sequences;
 set work_mem to '64MB', maintenance_work_mem to '256 MB';

cast type datetime to timestamptz drop default using zero-dates-to-null,
     type date drop not null drop default using zero-dates-to-null,
     type tinyint to boolean using tinyint-to-boolean;
~~~


And here's how to actually use it given the current very 
***alpha*** version of
the code that I have:

~~~
dim@darkstar:~/dev/CL/pgloader$ ./pgloader.lisp -f test/my.load 
Loading quicklisp and the pgloader project and its dependencies...
                    table name       read   imported     errors       time
------------------------------  ---------  ---------  ---------  ---------
       DROP then CREATE TABLES          0         26          0     0.410s
                  dolphin_list      44210      44210          0     1.183s
                       exclude        668        668          0      0.91s
                      category     994010     994010          0    21.114s
              to_check_species     469584     469584          0     8.940s
           to_check_screenshot        295        295          0     0.252s
             to_check_category      88774      88774          0     1.494s
                      to_check      41777      41777          0     2.728s
                          test         82         82          0      0.72s
                    other_test          3          3          0      0.28s
                          plop          3          3          0      0.20s
             matching_dolphins      42166      42166          0     1.140s
                           foo          3          3          0      0.39s
                       compare       3713       3713          0     0.218s
      DROP then CREATE INDEXES          0         48          0    14.818s
               RESET SEQUENCES          0         12          0      0.39s
------------------------------  ---------  ---------  ---------  ---------
          Total streaming time    1685288    1685288          0    52.586s
~~~


As you can see here we did import more than 
*one million and a half rows* in
*less than a minute*, in a local virtual machine running on top of my laptop
(optimized for travelling, not for data processing).

More important than those numbers that you can't easily compare to anything
else, the feature set makes it the tool I failed to find before. Given the
command above, the tool will actually connect to a MySQL database, retrieve
its table list and column definitions and apply 
***casting rules*** to those to be
able to issue the right 
`CREATE TABLE` statements in PostgreSQL.


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/type-casting-machine.640.jpg" >}}


Once the tables are in place in PostgreSQL, the data is 
***streamed*** from MySQL
to PostgreSQL using two concurrent threads and the 
**COPY** protocol at the
pushing side of the data 
*pipe*. Then the indexes (including 
*primary keys*) are
recreated and the sequences reset.

Lots of work remain to be done on that project, and that's the main reason
why I wanted to finish the bits I'd been sitting on for several months
already, so that you can see it running and share some motivation to see a
version 1.0 sooner rather than later.

The 
*TODO* list includes parallel loading of tables, per-column casting rules,
client-side 
*ddl-partitioning*, enhanced logging, binary distribution for a
bunch of platforms and lots of production battle testing. Oh and also adding
back the main 
[pgloader](/pgsql/pgloader) capabilities when loading from 
*flat files* (CSV, fixed
cols or otherwise) would be great too.

The battle testing is crucial given the source of the data: MySQL is known
for sometime introducing bugs in 
*minor* versions (I just had to fight against
[http://bugs.mysql.com/bug.php?id=19564](http://bugs.mysql.com/bug.php?id=19564) and hacked something to distinguish
NULL from empty string on the client), so the only way to convince oneself
that it works in any specific case is to test it.


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/dolphin-toy.320.jpg" >}}


*This one is not my favourite toy*...

If you're interested into that tool, as several people already told me they
are, then let's talk about how to reach version 1.0 together so that you
have a finished product that's easy to use, covering your bases, and Open
Source of course (to be released under 
*The PostgreSQL Licence)*!
