+++
date = "2013-08-02T10:19:00.000000+02:00"
title = "The Most Popular Pub Names"
tags = ["PostgreSQL", "KNN", "Extensions", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/openstreetmap.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/openstreetmap.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/08/02-pub-names-knn",
           "/blog/2013/08/02-pub-names-knn.html"]
+++

In his article titled 
[The Most Popular Pub Names](http://blog.mongodb.org/post/56876800071/the-most-popular-pub-names?utm_content=buffer4922c&utm_source=buffer&utm_medium=facebook&utm_campaign=Buffer) 
*Ross Lawley* did show us how
to perform some quite interesting 
*geographic queries* against 
[MongoDB](http://www.mongodb.org/), using
some nice 
*Open Data* found at the 
[Open Street Map](http://www.openstreetmap.org/) project.

<!--more-->

I found the idea behind that article really neat: using easily accessible
data produced by an Open Source project to show off some nice queries with
real data is what we should do more often. Also, as a PostgreSQL guy I
couldn't help but feel distracted by the query language used in that article
and thinking that it would be so much simpler to express the same queries in
SQL.

<!--toc-->

The idea behind SQL is that the syntax is made for the queries to be easy
enough to read and maintain while being insanely powerful at times. My take
on SQL is that it's often enough the easiest way to express your queries,
and that even if it can get crazy complex at times, that's only because SQL
has the power you need to pass all your complexity down to it.

At least that's my thinking, and this article is my try at sharing this
viewpoint with you.


# Loading the data

The data itself is available in some kind of an XML format where they
managed to handle the data in a 
[EAV](http://en.wikipedia.org/wiki/Entity%E2%80%93attribute%E2%80%93value_model) model:

~~~
<node id="262706" lat="51.0350300" lon="-0.7251785">
  <tag k="amenity" v="pub"/>
  <tag k="created_by" v="Potlatch 0.10f"/>
  <tag k="name" v="Kings Arms"/>
</node>
~~~


For the sake of this article, we're going to use the simplest schema
possible. I didn't want to try and see if the 
`id` actually is unique and
never omitted, for example, so here's the schema I've been working with:

~~~ sql
create table if not exists pubnames (id bigint, pos point, name text);
~~~


Where the 
*MongoDB* article used 
[imposm python library](http://imposm.org/) to load the data, I
wanted to take the opportunity to loading it as a stream: a 
[SAX](http://common-lisp.net/project/cxml/klacks.html) like API to
read the XML should allow to send the data as we parse it in a 
`COPY` stream,
right?

Here's a slightly edited portion of the code I've been using to parse and
load the data, available as the 
[pubnames](https://github.com/dimitri/pubnames) project on 
*GitHub*:

~~~ lisp
(defun parse-osm-file (&key
			 (pathname *pub-xml-pathname*)
			 (truncate t)
			 (drop nil))
  "Parse the given PATHNAME file, formated as OSM XML."
  (maybe-create-postgresql-table :drop drop :truncate truncate)

  (klacks:with-open-source (s (cxml:make-source pathname))
    (loop
       with stream =
	 (cl-postgres:open-db-writer (remove :port *pgconn*) *pg-table-name* nil)
       for key = (klacks:peek s)
       while key
       do
	 (case key
	   (:start-element (parse-osm-start-element s))
           ; parse-osm-end-element calls cl-postgres:db-write-row
	   (:end-element   (parse-osm-end-element s stream)))
	 (klacks:consume s)

       finally (return (cl-postgres:close-db-writer stream)))))
~~~


Given that code, we can parse the data in the XML file and load it into our
PostgreSQL table in about 5.5 seconds on my laptop. If I had to optimize
that loading time I'd try having two concurrent threads, one of them reading
from the XML file and pushing to a queue, and the other one pulling from the
same queue and filling our 
`COPY` buffer.


# Normalizing the data

The first query of our reference article 
[The Most Popular Pub Names](http://blog.mongodb.org/post/56876800071/the-most-popular-pub-names?utm_content=buffer4922c&utm_source=buffer&utm_medium=facebook&utm_campaign=Buffer) shows
the python code they've been using in order to normalize the data so that
it's then possible to list 
***The Most Popular Pub Names*** in the United Kingdom.
Here, we didn't process the OSM data at all, so what about normalizing it
directly within a SQL query?

~~~
  select array_to_string(array_agg(distinct(name) order by name), ', '),
         count(*)
    from pubnames
group by replace(replace(name, 'The ', ''), 'And', '&')
order by 2 desc
limit 5;

       array_to_string        | count 
------------------------------+-------
 Red Lion, The Red Lion       |   350
 Royal Oak, The Royal Oak     |   287
 Crown, The Crown             |   204
 The White Hart, White Hart   |   180
 The White Horse, White Horse |   163
(5 rows)

Time: 152.786 ms
~~~


The 
`array_to_string` function allows us to tweak the output to our
convenience, as the 
`array_agg(distinct(name) order by name)` aggregate is
doing all the work for us here, in grouping all 
*names* together and keeping
an ordered set of a unique entry per variant.

Which 
*names* do we group together will you ask me? Well, those having the
same name apart from some spelling variants: we don't want to consider 
`The`
to be a difference so we replace it with an empty string, and we do want to
consider both 
`And` and 
`&` as the same thing too.

Again, I'm reproducing the same processing as with the 
*MongoDB* article.


# Geolocating nearest pub (KNN search)

The spelling of the 
[KNN](https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm) search in 
[PostgreSQL](http://www.postgresql.org/) involves ordering the result
set with a 
*distance* operator, which is itself spelled 
`<->`. Here's the full
SQL for searching the pubs nearby our position, or actually the position
given as an example in the 
*MongoDB* article:

~~~ sql
  select id, name, pos
    from pubnames
order by pos <-> point(51.516,-0.12)
   limit 3;

     id     |          name          |           pos           
------------+------------------------+-------------------------
   21593238 | All Bar One            | (51.5163499,-0.1192746)
   26848690 | The Shakespeare's Head | (51.5167871,-0.1194731)
  371049718 | The Newton Arms        | (51.5163032,-0.1209811)
(3 rows)

Time: 18.679 ms
~~~


As we're using the 
[point datatype](http://www.postgresql.org/docs/current/interactive/datatype-geometric.html#AEN6473) in PostgreSQL, there's no simple way that
I know of to convert that distance into something like 
*meters* or maybe 
*yards*
here. That's of course possible to do, even considering the actual shape of
the earth, thanks to some 
[PostgreSQL Extensions](http://www.postgresql.org/docs/current/interactive/extend-extensions.html) such as 
[earthdistance](http://www.postgresql.org/docs/9.2/interactive/earthdistance.html) or the
full blown 
[PostGIS](http://postgis.net/). The details about that are for another article though.

PostgreSQL has a very rich and powerful datatype system that goes well
beyond storing numbers, text and dates. If you're not familiar with that
idea, you should read about it, maybe beginning with 
[PostgreSQL Data Types](http://www.postgresql.org/docs/current/interactive/datatype.html)
chapter of the documentation.


# Using a KNN specific index


{{< image classes="fig50 right fancybox dim-margin"
              src="/img/gist_sample-320.png" >}}

With a dataset of 27878 rows having an answer in about 20ms is not a great
achievement. Indeed, we didn't create any indexing whatsoever on the table
yet, so the query planner has no other choice but to scan the whole content
on disk and filter it as it goes.

It would be way better for performances if we could instead evaluate our
query constraints (here, the 
`ORDER BY` and 
`LIMIT` clauses) using some index
search instead.

That's exactly the kind of situation that 
[GiST](http://www.postgresql.org/docs/9.2/interactive/gist.html) and 
[SP-GiST](http://www.postgresql.org/docs/9.2/interactive/spgist.html) indexes have been
designed to be able to solve for you in PostgreSQL, and in particular the
KNN GiST support. Let's have a try at it:

~~~ sql
> create index on pubnames using gist(pos);

> select id, name, pos
    from pubnames
order by pos <-> point(51.516,-0.12) limit 3;
     
     id     |          name          |           pos           
------------+------------------------+-------------------------
   21593238 | All Bar One            | (51.5163499,-0.1192746)
   26848690 | The Shakespeare's Head | (51.5167871,-0.1194731)
  371049718 | The Newton Arms        | (51.5163032,-0.1209811)
(3 rows)

Time: 0.849 ms
~~~


Now we talk! With a dataset of 27878 rows in total, finding the 3 nearest
pubs in less than a millisecond is something we can actually be happy with,
and can use directly in a web application. I would expect this performances
to remain in the right ballpark even for a much larger dataset, and leave it
as an exercise for you to find that dataset and test the 
***KNN GiST*** indexes on
it!


# Conclusion

{{< image classes="fig50 right fancybox dim-margin"
              src="/img/fdws.320.png"
            title="PostgreSQL is at the center of your dataverse">}}

What I want to take home from this article is the idea that the plain old
SQL language still has lots to offer to modern data analysis needs, in
particular when you're using 
[PostgreSQL](http://www.postgresql.org/).

That database system knows how to stay relevant in a fast evolving
environment, where your needs are more and more demanding: more data, more
analysis, more users.

The past few releases of PostgreSQL each come with plenty of new features to
better support your demanding use cases. We do a solid new 
[release](http://www.postgresql.org/support/versioning/) each
year, and you can check the 
[feature matrix](http://www.postgresql.org/about/featurematrix/) to see by yourself the amazing
pace at which we are able to improve our system.

If you're using 
[PostgreSQL](http://www.postgresql.org/) you have very few reasons to look for another
solution. Some cases of course are still best handled in system more
tolerant of data loss for example. When that happen though, in my
experience, it's always a complementary service that will run alongside
PostgreSQL. And for them to coexist peacefully, we even offer you
[Foreign Data Wrappers](http://wiki.postgresql.org/wiki/Foreign_data_wrappers)!
