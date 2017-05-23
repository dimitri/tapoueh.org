+++
date = "2010-09-13T17:45:00.000000+02:00"
title = "switch-window reaches 0.8"
tags = ["Emacs", "el-get", "release", "switch-window", "cssh"]
categories = ["Projects","switch-window"]
thumbnailImage = "/img/old/el-get.big.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/el-get.big.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/09/13-switch-window-reaches-08",
           "/blog/2010/09/13-switch-window-reaches-08.html"]
+++

I wanted to play with the idea of using the whole keyboard for my
[switch-window](http://github.com/dimitri/switch-window) utility, but wondered how to get those keys in the right order
and all. Finally found 
`quail-keyboard-layout` which seems to exists for such
uses, as you can see:

~~~
(loop with layout = (split-string quail-keyboard-layout "") 
  for row from 1 to 4
  collect (loop for col from 1 to 12
 ("q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]")
 ("a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'" "\\")
 ("z" "x" "c" "v" "b" "n" "m" "," "." "/" " " " "))
~~~


So now 
`switch-window` will use that (but only the first 
`10` letters) instead
of 
*hard-coding* numbers from 1 to 9 as labels and direct switches. That makes
it more suitable to 
[cssh](http://github.com/dimitri/cssh) users too, I guess.

In other news, I think 
[el-get](http://github.com/dimitri/el-get) is about ready for its 
`1.0` release. Please
test it and report any problem very soon before the release!
