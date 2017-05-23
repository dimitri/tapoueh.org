+++
date = "2010-08-26T17:45:00.000000+02:00"
title = "Playing with bit strings"
tags = ["PostgreSQL", "Tricks", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/bitstring_logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/bitstring_logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/08/26-playing-with-bit-strings",
           "/blog/2010/08/26-playing-with-bit-strings.html"]
+++

The idea of the day ain't directly from me, I'm just helping with a very
thin subpart of the problem. The problem, I can't say much about, let's just
assume you want to reduce the storage of 
`MD5` in your database, so you want
to abuse 
[bit strings](http://www.postgresql.org/docs/8.4/interactive/datatype-bit.html). A solution to use them works fine, but the datatype is
still missing some facilities, for example going from and to hexadecimal
representation in text.


~~~
create or replace function hex_to_varbit(h text)
 returns varbit
 language sql
as $$
  select ('X' || $1)::varbit;
$$;

create or replace function varbit_to_hex(b varbit)
 returns text
 language sql
as $$
  select array_to_string(array_agg(to_hex((b << (32*o))::bit(32)::bigint)), '')
    from (select b, generate_series(0, n-1) as o
            from (select $1, octet_length($1)/4) as t(b, n)) as x
$$;
~~~


To understand the magic in the second function, let's walk through the tests
one could do when wanting to grasp how things work in the 
`bitstring` world
(using also some reading of the fine documentation, too).

~~~
=# select ('101011001011100110010110'::varbit << 0)::bit(8);
   bit    
----------
 10101100
(1 row)

=# select ('101011001011100110010110'::varbit << 8)::bit(8);
   bit    
----------
 10111001
(1 row)

=# select ('101011001011100110010110'::varbit << 16)::bit(8);
   bit    
----------
 10010110
(1 row)

=# select * from *TEMP VERSION OF THE FUNCTION FOR TESTING*
 o |                b                 |    x     
---+----------------------------------+----------
 0 | 10101100101111010001100011011011 | acbd18db
 1 | 01001100110000101111100001011100 | 4cc2f85c
 2 | 11101101111011110110010101001111 | edef654f
 3 | 11001100110001001010010011011000 | ccc4a4d8
(4 rows)
~~~


What do we get from that, will you ask? Let's see a little example:

~~~
=# select hex_to_varbit(md5('foo'));
                                                          hex_to_varbit                                                           
----------------------------------------------------------------------------------------------------------------------------------
 10101100101111010001100011011011010011001100001011111000010111001110110111101111011001010100111111001100110001001010010011011000
(1 row)

=# select md5('foo'), varbit_to_hex(hex_to_varbit(md5('foo')));
               md5                |          varbit_to_hex           
----------------------------------+----------------------------------
 acbd18db4cc2f85cedef654fccc4a4d8 | acbd18db4cc2f85cedef654fccc4a4d8
(1 row)
~~~


Storing 
`varbits` rather than the 
`text` form of the 
`MD5` allows us to go from
`6510 MB` down to 
`4976 MB` on a sample table containing 100 millions
rows. We're targeting more that that, so that's a great win down here!

In case you wonder, querying the main index on 
`varbit` rather than the one on
`text` for a single result row, the cost of doing the conversion with
`varbit_to_hex` seems to be around 
`28 &#xB5;s`. We can afford it.

Hope this helps!
