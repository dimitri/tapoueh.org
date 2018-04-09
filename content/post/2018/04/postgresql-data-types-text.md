+++
title = "PostgreSQL Data Types: Text Encoding"
date = "2018-04-09T13:33:01+02:00"
tags = ["PostgreSQL","YeSQL","Data Types","Text","Encoding"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/encodings.png"
coverMeta = "out"
coverSize = "partial"
thumbnailImage = "/img/220px-Wikipedia_Logo_1.0.png"
thumbnailImagePosition = "left"

+++

Continuing our series of [PostgreSQL Data Types](/tags/data-types/) today
we're going to introduce the PostgreSQL text data type. The first notion to
understand when processing text in any program is of course the notion of
encoding.

So when addressing the text datatype we must mention encoding settings, and
possibly also issues. An encoding is a particular representation of
characters in bits and bytes. In the ASCII encoding the letter `A` is
encoded as the 7-bits byte `1000001`, or 65 in decimal, or 41 in
hexadecimal. All those numbers are going to be written the same way on-disk,
and the letter `A` too.

<!--more-->
<!--toc-->

# PostgreSQL Server Side Encoding

To know which encoding your database is using, run the *psql* command `\l`:

~~~ psql
                                  List of databases
   Name    │  Owner   │ Encoding │   Collate   │    Ctype    │ …
═══════════╪══════════╪══════════╪═════════════╪═════════════╪═
 chinook   │ dim      │ UTF8     │ en_US.UTF-8 │ en_US.UTF-8 │ …
 f1db      │ dim      │ UTF8     │ en_US.UTF-8 │ en_US.UTF-8 │ …
 pgloader  │ dim      │ UTF8     │ en_US.UTF-8 │ en_US.UTF-8 │ …
 template0 │ postgres │ UTF8     │ en_US.UTF-8 │ en_US.UTF-8 │ …
 template1 │ postgres │ UTF8     │ en_US.UTF-8 │ en_US.UTF-8 │ …
 yesql     │ dim      │ UTF8     │ en_US.UTF-8 │ en_US.UTF-8 │ …
(6 rows)
~~~

In this output, I've stripped down the last column of output for better
integration for the page size here, so you don't get to see the *Access
privileges* for those databases.

The encoding here is UTF8 which is the best choice these days, and you can
see that the collation and ctype are English based in the UTF-8 encoding,
which is good for my installation. You might, of course, pick something
else.

The non-encoding SQL\_ASCII accepts any data you throw at it, whereas the
UTF8 encoding (and some others) do check for valid input. Never use
SQL_ASCII, as you will not be able to retrieve data in any encoding and will
lose data because of that! Migrating away from SQL_ASCII to a proper
encoding such as UTF8 is possible but lossy and complex.

{{< alert info >}}

You can read my articles [Getting out of SQL_ASCII, part
1](/blog/2010/02/getting-out-of-sql_ascii-part-1/) and [Getting out of
SQL_ASCII, part 2](/blog/2010/02/getting-out-of-sql_ascii-part-2/) that I
wrote in 2010 when I had to get out of this tricky situation.

{{< /alert >}}

# PostgreSQL Client Side Encoding

You can also have an UTF8 encoded database and use a legacy application (or
programming language) that doesn't know how to handle Unicode properly. In
that case, you can ask PostgreSQL to convert all and any data on the fly
between the server-side encoding and your favorite client-side encoding,
thanks to the *client_encoding* setting.

~~~ sql
show client_encoding;
~~~

Here again, we use UTF8 client side, which allows handling French
accentuated characters, for instance.

~~~ psql
 client_encoding 
═════════════════
 UTF8
(1 row)
~~~

Be aware that not all combinations of *server encoding* and *client
encoding* make sense. While it is possible for PostgreSQL to communicate
with your application using the *latin1* encoding on the client side, if the
server side dataset includes texts in incompatible encodings, PostgreSQL
will issue an error. Such texts might be written using non-Latin scripts
such as Cyrillic, Chinese, Japanese, Arabic or other languages.

From the Emacs facility `M-x view-hello-file`, here's a table with spelling
of hello in plenty of different languages and scripts, all encoded in
*UTF8*:

~~~ psql
          language          │            hello        
════════════════════════════╪═════════════════════════════
 Czech (čeština)            │ Dobrý den
 Danish (dansk)             │ Hej / Goddag / Halløj
 Dutch (Nederlands)         │ Hallo / Dag
 English /ˈɪŋɡlɪʃ/          │ Hello
 Esperanto                  │ Saluton (Eĥoŝanĝo ĉiuĵaŭde)
 Estonian (eesti keel)      │ Tere päevast / Tere õhtust
 Finnish (suomi)            │ Hei / Hyvää päivää
 French (français)          │ Bonjour / Salut
 Georgian (ქართველი)        │ გამარჯობა
 German (Deutsch)           │ Guten Tag / Grüß Gott
 Greek (ελληνικά)           │ Γειά σας
 Greek, ancient (ἑλληνική)  │ Οὖλέ τε καὶ μέγα χαῖρε
 Hungarian (magyar)         │ Szép jó napot!
 Italian (italiano)         │ Ciao / Buon giorno
 Maltese (il-Malti)         │ Bonġu / Saħħa
 Mathematics                │ ∀ p ∈ world • hello p  □
 Mongolian (монгол хэл)     │ Сайн байна уу?
 Norwegian (norsk)          │ Hei / God dag
 Polish  (język polski)     │ Dzień dobry! / Cześć!
 Russian (русский)          │ Здра́вствуйте!
 Slovak (slovenčina)        │ Dobrý deň
 Slovenian (slovenščina)    │ Pozdravljeni!
 Spanish (español)          │ ¡Hola!
 Swedish (svenska)          │ Hej / Goddag / Hallå
 Turkish (Türkçe)           │ Merhaba
 Ukrainian (українська)     │ Вітаю
 Vietnamese (tiếng Việt)    │ Chào bạn
 Japanese (日本語)          │ こんにちは / ｺﾝﾆﾁﾊ
 Chinese (中文,普通话,汉语) │ 你好
 Cantonese (粵語,廣東話)    │ 早晨, 你好
~~~

Now, of course, I can't have that data sent to me in *latin1*:

~~~ psql
yesql# set client_encoding to latin1;
SET
yesql# select * from hello where language ~ 'Georgian';
ERROR:  character with byte sequence 0xe1 0x83 0xa5 in encoding "UTF8" ⏎
has no equivalent in encoding "LATIN1"
yesql# reset client_encoding ;
RESET
~~~

So if it's possible for you, use *UTF-8* encoding and you'll have a much
simpler life. It must be noted that Unicode encoding makes comparing and
sorting text a rather costly operation. That said being fast and wrong is
not an option, so we are going to still use unicode text!

# Conclusion

{{< figure class="right"
             src="/img/MasteringPostgreSQLinAppDev-Cover-th.png"
            link="https://masteringpostgresql.com" >}}
            
This article is an extract from my book [Mastering PostgreSQL in Application
Development](https://masteringpostgresql.com), which teaches SQL to
developers so that they may replace thousands of lines of code with very
simple queries. The book has a full chapter about data types in PostgreSQL,
check it out!
