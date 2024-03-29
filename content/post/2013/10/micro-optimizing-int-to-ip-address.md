+++
date = "2013-10-03T22:10:00.000000+02:00"
title = "A Worthwile Micro Optimisation"
tags = ["PostgreSQL", "pgloader", "ip4r", "Extensions", "Common-Lisp"]
categories = ["Projects","pgloader"]
thumbnailImage = "/img/old/make-computer-faster.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/make-computer-faster.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2013/10/03-micro-optimizing-int-to-ip-address",
           "/blog/2013/10/03-micro-optimizing-int-to-ip-address.html"]
+++

In our previous article about 
[Loading Geolocation Data](/blog/2013/10/01-loading-geolocation-data), we did load some
data into PostgreSQL and saw the quite noticable impact of a user
transformation. As it happens, the function that did the integer to IP
representation was so naive as to scratch the micro optimisation itch of
some Common Lisp hackers: thanks a lot guys, in particular 
[stassats](https://github.com/stassats) who came
up with the solution we're seeing now.

<!--more-->

The previous code was a straight rewrite of the provided documentation in
Common Lisp. See for yourself the docs as found at
[http://dev.maxmind.com/geoip/legacy/csv/](http://dev.maxmind.com/geoip/legacy/csv/):

~~~
integer_ip = 2921648058

o1 = int ( ipnum / 16777216 ) % 256;
o2 = int ( ipnum / 65536    ) % 256;
o3 = int ( ipnum / 256      ) % 256;
o4 = int ( ipnum            ) % 256;

address = ( o1, o2, o3, o4 ).join('.')
~~~


And the code I came up with:

~~~ lisp
(defun ip-range (start-integer-string end-integer-string)
  "Transform a couple of integers to an IP4R ip range notation."
  (declare (inline)
	   (optimize speed)
	   (type string start-integer-string end-integer-string))
  (flet ((integer-to-ip-string (int)
	   "see http://dev.maxmind.com/geoip/legacy/csv/"
	   (declare (inline) (optimize speed) (type fixnum int))
	   (format nil "~a.~a.~a.~a"
		   (mod (truncate int #. (expt 2 24)) 256)
		   (mod (truncate int #. (expt 2 16)) 256)
		   (mod (truncate int #. (expt 2 8)) 256)
		   (mod int 256))))
    (let ((ip-start (integer-to-ip-string (parse-integer start-integer-string)))
	  (ip-end   (integer-to-ip-string (parse-integer end-integer-string))))
      (format nil "~a-~a" ip-start ip-end))))
~~~


Quite a direct naive implementation. Which is good to show what you can
expect in a kind of a worst case, and that worst case was using 
*31.546
seconds* rather than 
*17.425 seconds* when not doing any conversion. Well of
course the python code was spending 
*78.979 seconds* for not doing any
conversion, but that's not the topic today.

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/ip-address.640.png" >}}
</center>

Let's now see one of the micro optimised solution, the one I picked among a
list of 8 different proposal, each a little more crazy than the previous
one:

~~~ lisp
(declaim (inline int-to-ip))
(defun int-to-ip (int)
  "Transform an IP as integer into its dotted notation, optimised code from
   stassats."
  (declare (optimize speed)
           (type (unsigned-byte 32) int))
  (let ((table (load-time-value
                (let ((vec (make-array (+ 1 #xFFFF))))
                  (loop for i to #xFFFF
		     do (setf (aref vec i)
			      (coerce (format nil "~a.~a"
					      (ldb (byte 8 8) i)
					      (ldb (byte 8 0) i))
				      'simple-base-string)))
                  vec)
                t)))
    (declare (type (simple-array simple-base-string (*)) table))
    (concatenate 'simple-base-string
		 (aref table (ldb (byte 16 16) int))
		 "."
		 (aref table (ldb (byte 16 0) int)))))

(declaim (inline ip-range))
(defun ip-range (start-integer-string end-integer-string)
  "Transform a couple of integers to an IP4R ip range notation."
  (declare (optimize speed)
	   (type string start-integer-string end-integer-string))
  (let ((ip-start (int-to-ip (parse-integer start-integer-string)))
	(ip-end   (int-to-ip (parse-integer end-integer-string))))
    (concatenate 'simple-base-string ip-start "-" ip-end)))
~~~


As usual the idea is to compute all you can in advance, here thanks to the
[load-time-value](http://www.lispworks.com/documentation/HyperSpec/Body/s_ld_tim.htm) special operator that's part of the Common Lisp Standard. So
we compute a table of all the dotted representation for a pair of two bytes,
and we do that computation at 
*load time*, which happens when you load the
*compiled code artifact* you generated from your sources. Then all we have to
do is take the upper and lower bytes, fetch their representation in our
table, and concatenate both with a middle dot.

The reason why we only keep 2 bytes in the table is so that we don't require
about 
*64 GB* of memory to be able to transform ip addresses...

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/cpu-ram-disk.640.jpg" >}}
</center>

<center>*Even after all those years, either compute again or store in memory.*</center>

And what do we have now?

~~~
                    table name       read   imported     errors       time
------------------------------  ---------  ---------  ---------  ---------
                       extract          0          0          0       1.01
                   before load          0          0          0      0.077
------------------------------  ---------  ---------  ---------  ---------
              geolite.location     438386     438386          0     10.352
                geolite.blocks    1790461    1790461          0     18.045
------------------------------  ---------  ---------  ---------  ---------
                       finally          0          0          0     31.108
------------------------------  ---------  ---------  ---------  ---------
             Total import time    2228847    2228847          0   1m0.592s
~~~


Thanks to the optimisation, the 
*two bigint as text to iprange as text*
transformation now has an added cost of 
`620 ms` with our data set. The whole
file loading is now averaging to 
`10.07841 µs` per row, or just a tad more
than 
***10 microseconds*** per row, transformation included this time.

Less than a second of added cost within a complete process taking around a
minute, that basically means that the transformation is now free.

Despite what you might have read elsewhere, my experience with the Common
Lisp Community so far really is great!
