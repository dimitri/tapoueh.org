+++
date = "2011-08-05T11:30:00.000000+02:00"
title = "pgloader reformating"
tags = ["PostgreSQL", "pgloader"]
categories = ["Projects","pgloader"]
thumbnailImage = "/img/old/toy-loader.320.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/toy-loader.320.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/08/05-reformating-modules-for-pgloader",
           "/blog/2011/08/05-reformating-modules-for-pgloader.html"]
+++

Back to our series about [pgloader](http://pgloader.io). The previous
articles
detailed
[How To Use PgLoader](http://tapoueh.org/blog/2011/07/22-how-to-use-pgloader.html) then
[How to Setup pgloader](http://tapoueh.org/blog/2011/07/29-how-to-setup-pgloader.html),
then what to expect from
a
[parallel pgloader](http://tapoueh.org/blog/2011/08/01-parallel-pgloader.html) setup.
This article will detail how to *reformat* input columns so that
what [PostgreSQL](http://www.postgresql.org/) sees is not what's in the data
file, but the result of a *transformation* from this data into something
acceptable as an *input* for the target data type.

<!--more-->

{{< alert danger >}}

This article is about versions 2.x of pgloader, which are not supported
anymore. Consider using [pgloader](http://pgloader.io) version 3.x instead.

{{< /alert >}}


Here's what
the [pgloader documentation](http://pgloader.io/howto/pgloader.1.html) has
to say about this *reformat* parameter: *The value of this option is a comma
separated list of columns to rewrite, which are a colon separated list of
column name, reformat module name, reformat function name*.

And here's the 
[examples/pgloader.conf](https://github.com/dimitri/pgloader/blob/master/examples/pgloader.conf) section that deals with reformat:

~~~ ini
[reformat]
table           = reformat
format          = text
filename        = reformat/reformat.data
field_sep       = |
columns         = id, timestamp
reformat        = timestamp:mysql:timestamp
~~~


The documentation says some more about it, so check it out.  Also, the
`reformat_path` option (set either on the command line or in the configuration
file) is used to find the python module implementing the reformat function.
Please refer to the manual as to how to set it.

Now, obviously, for the 
*reformat* to happen we need to write some code.
That's the whole point of the option: you need something very specific, you
are in a position to write the 5 lines of code needed to make it happen,
[pgloader](http://tapoueh.org/pgsql/pgloader.html) allows you to just do that.  Of course, the code needs to be
written in python here, so that you can even benefit from the
[parallel pgloader](http://tapoueh.org/blog/2011/08/01-parallel-pgloader.html) settings.

Let's see an reformat module exemple, as found in 
[reformat/mysql.py](https://github.com/dimitri/pgloader/blob/master/reformat/mysql.py) in the
`pgloader` sources:

~~~
# Author: Dimitri Fontaine <dim@tapoueh.org>
#
# pgloader mysql reformating module
#

def timestamp(reject, input):
    """ Reformat str as a PostgreSQL timestamp

    MySQL timestamps are like:  20041002152952
    We want instead this input: 2004-10-02 15:29:52
    """
    if len(input) != 14:
        e = "MySQL timestamp reformat input too short: %s" % input
        reject.log(e, input)
    
    year    = input[0:4]
    month   = input[4:6]
    day     = input[6:8]
    hour    = input[8:10]
    minute  = input[10:12]
    seconds = input[12:14]
    
    return '%s-%s-%s %s:%s:%s' % (year, month, day, hour, minute, seconds)
~~~


This reformat module will 
*transform* a 
`timestamp` representation as issued by
certain versions of MySQL into something that PostgreSQL is able to read as
a timestamp.

If you're in the camp that wants to write as little code as possible rather
than easy to read and maintain code, I guess you could write it this way
instead:

~~~
import re
def timestamp(reject, input):
    """ 20041002152952 -> 2004-10-02 15:29:52 """
    g = re.match(r"(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})", input)
    return '%s-%s-%s %s:%s:%s' % tuple([g.group(x+1) for x in range(6)])
~~~


Whenever you have an input file with data that PostgreSQL chokes upon, you
can solve this problem from 
[pgloader](http://tapoueh.org/pgsql/pgloader.html) itself: no need to resort to scripting
and a pipelines of 
[awk](http://www.gnu.org/software/gawk/manual/gawk.html) (which I use a lot in other cases, don't get me
wrong) or other tools.  See, you finally have an excuse to 
[Dive into Python](http://diveintopython.org/)!
