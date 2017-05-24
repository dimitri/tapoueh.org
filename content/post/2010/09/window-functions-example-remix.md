+++
date = "2010-09-12T21:35:00.000000+02:00"
title = "Window Functions example remix"
tags = ["PostgreSQL", "YeSQL"]
categories = ["PostgreSQL","YeSQL"]
thumbnailImage = "/img/old/remix.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/remix.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/09/12-window-functions-example-remix",
           "/blog/2010/09/12-window-functions-example-remix.html"]
+++

The drawback of hosting a static only website is, obviously, the lack of
comments. What happens actually, though, is that I receive very few comments
by direct mail. As I don't get another 
*spam* source to cleanup, I'm left
unconvinced that's such a drawback. I still miss the low probability of
seeing blog readers exchange directly, but I think a 
`tapoueh.org` mailing
list would be my answer, here...

Anyway, 
[David Fetter](http://people.planetpostgresql.org/dfetter/) took the time to send me a comment by mail with a
cleaned up rewrite of the previous entry 
`SQL`, here's it for your pleasure!

~~~
WITH t AS (
    SELECT
        o, w,
        CASE WHEN
            LAG(w) OVER(w) IS DISTINCT FROM w AND
            ROW_NUMBER() OVER (w) > 1 /* Eliminate first change */
        THEN 1
        END AS change
    FROM (
        VALUES
            (1, 5),
            (2, 10),
            (3, 7),
            (4, 7),
            (5, 7)
    ) AS data(o, w)
    WINDOW w AS (ORDER BY o) /* Factor out WINDOW */
)
SELECT SUM(change) FROM t;
~~~


As you can see 
***David*** chose to filter the first change in the subquery rather
than hacking it away with a simple 
`-1` at the outer level. I'm still
wondering which way is cleaner (that depends on how you look at the
problem), but I think I know which one is simpler! Thanks 
***David*** for this
blog entry!
