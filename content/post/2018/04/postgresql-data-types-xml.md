+++
title = "PostgreSQL Data Types: XML"
date = "2018-04-23T18:18:48+02:00"
tags = ["PostgreSQL","YeSQL","Data Types","XML"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/digital-transformation.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/XML-icon.png"
thumbnailImagePosition = "left"

+++

Continuing our series of [PostgreSQL Data Types](/tags/data-types/) today
we're going to introduce the PostgreSQL XML type.

The SQL standard includes a [SQL/XML](https://en.wikipedia.org/wiki/SQL/XML)
which _introduces the predefined data type XML together with constructors,
several routines, functions, and XML-to-SQL data type mappings to support
manipulation and storage of XML in a SQL database_, as per the Wikipedia
page.

<!--more-->
<!--toc-->

## The XML Data Type

PostgreSQL implements the XML data type, which is documented in the chapters
on [XML
type](https://www.postgresql.org/docs/current/static/datatype-xml.html) and
[XML
functions](https://www.postgresql.org/docs/current/static/functions-xml.html)
chapters.

The best option when you need to process XML documents might be the
[XSLT](https://en.wikipedia.org/wiki/XSLT) transformation language for XML.
It should be no surprise that a PostgreSQL extension allows writing *stored
procedures* in this language. If you have to deal with XML documents in your
database, check out [PL/XSLT](https://github.com/petere/plxslt).

## Processing XML with PL/XSLT

An example of a *PL/XSLT* function follows:

~~~ sql
create extension plxslt;

CREATE OR REPLACE FUNCTION striptags(xml) RETURNS text
	LANGUAGE xslt
AS $$<?xml version="1.0"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns="http://www.w3.org/1999/xhtml"
>

  <xsl:output method="text" omit-xml-declaration="yes"/>

  <xsl:template match="/">
    <xsl:apply-templates/>
  </xsl:template>

</xsl:stylesheet>
$$;
~~~

It can be used like this:

~~~ sql
create table docs
 (
   id      serial primary key,
   content xml
 );

insert into docs(content)
     values ('<?xml version="1.0"?>
<html xmlns="http://www.w3.org/1999/xhtml">
<body>hello</body>
</html>');

select id, striptags(content)
  from docs;
~~~

As expected, here's the result:

~~~ psql
 id │ striptags 
════╪═══════════
  1 │          ↵
    │ hello    ↵
    │ 
(1 row)
~~~

## Conclusion

The XML support in PostgreSQL might be handy in cases. It's mainly been
added for standard compliance, though, and is not found a lot in the field.
XML processing function and XML indexing is pretty limited in PostgreSQL.

{{< figure class="right"
             src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
            link="https://masteringpostgresql.com" >}}
            
This article is an extract from my book [Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com), which teaches SQL to
developers so that they may replace thousands of lines of code with very
simple queries. The book has a full chapter about data types in PostgreSQL,
check it out!

