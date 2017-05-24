+++
date = "2009-02-05T00:00:00.000000+01:00"
title = "Importing XML content from file"
tags = ["PostgreSQL", "plpgsql", "tricks", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/xml-to-db-simple.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/xml-to-db-simple.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2009/02/05-importing-xml-content-from-file",
           "/blog/2009/02/05-importing-xml-content-from-file.html"]
+++

The problem was raised this week on 
[IRC](http://www.postgresql.org/community/irc) and this time again I felt it would
be a good occasion for a blog entry: how to load an 
`XML` file content into a
single field?

The usual tool used to import files is 
[COPY](http://www.postgresql.org/docs/current/interactive/sql-copy.html), but it'll want each line of the
file to host a text representation of a database tuple, so it doesn't apply
to the case at hand. 
[RhodiumToad](http://blog.rhodiumtoad.org.uk/) was online and offered the following code
to solve the problem:

~~~
create or replace function xml_import(filename text)
  returns xml
  volatile
  language plpgsql as
$f$
    declare
        content bytea;
        loid oid;
        lfd integer;
        lsize integer;
    begin
        loid := lo_import(filename);
        lfd := lo_open(loid,262144);
        lsize := lo_lseek(lfd,0,2);
        perform lo_lseek(lfd,0,0);
        content := loread(lfd,lsize);
        perform lo_close(lfd);
        perform lo_unlink(loid);
 
        return xmlparse(document convert_from(content,'UTF8'));
    end;
$f$;
~~~


As you can see, the trick here is to use the 
[large objects](http://www.postgresql.org/docs/current/interactive/largeobjects.html) API to load the
file content into memory (
`content` variable), then to parse it knowing it's
an 
`UTF8` encoded 
`XML` file and return an 
[XML](http://www.postgresql.org/docs/current/interactive/datatype-xml.html) datatype object.
