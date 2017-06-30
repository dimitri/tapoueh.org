+++
date = "2014-01-11T21:34:00.000000+01:00"
title = "El-Get is now Rolling Releases"
tags = ["El-Get", "Emacs"]
categories = ["Emacs","el-get"]
thumbnailImage = "/img/old/el-get.big.png"
thumbnailImagePosition = "left"
coverImage = "/img/pangolin.jpg"
coverSize = "partial"
coverMeta = "in"
aliases = ["/blog/2014/01/11-el-get-rolling-release",
           "/blog/2014/01/11-el-get-rolling-release.html"]
+++

The code of 
[El-Get](https://github.com/dimitri/el-get) has been pretty stable for a long time now. About the
whole set of patches against the 
`4.x` series has been about bug fixing corner
cases (sometimes cases that had nothing 
*cornery* about them too) and
providing more and more recipes. That's what you expect from a 
***stable***
software, and that what allows us to move to a 
*rolling releases* model.

In practice, it means that you won't have to suffer from using a badly
maintained 
*stable* branch anymore. The El-Get 
*scratch installer* is now
targetting the 
*master* branch of the git repository, and this branch is now
going to only accept either bug fixes or 
*proven* patches.

In case something big and destabilizing is proposed, we will of course
consider doing a new release, after 5 comes 6. Meanwhile, enjoy your new
El-Get stable branch, it's officially the 
*master* one!

> If you're currently using El-Get, make sure you're using the 


To upgrade El-Get to the master's branch, the simplest way might as well be
working with git here:

~~~
M-x el-get-cd RET el-get
M-! git checkout master
M-x el-get-self-update
~~~


*(Restart Emacs and*) Enjoy El-Get Rolling Release!
