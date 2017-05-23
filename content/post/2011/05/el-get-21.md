+++
date = "2011-05-26T10:00:00.000000+02:00"
title = "el-get 2.1"
tags = ["Emacs", "el-get", "release"]
categories = ["Emacs","el-get"]
thumbnailImage = "/img/old/el-get.big.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/el-get.big.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/05/26-el-get-21",
           "/blog/2011/05/26-el-get-21.html"]
+++

Current 
[el-get](https://github.com/dimitri/el-get) status is stable, ready for daily use and packed with extra
features that make life easier.  There are some more things we could do, as
always, but they will be about smoothing things further.


## Latest released version

[el-get](https://github.com/dimitri/el-get) version 
`2.1` is available, with a boatload of features, including
autoloads support, byte-compiling in an external 
*clean room* 
[Emacs](http://www.gnu.org/software/emacs/) instance,
custom support, lazy initialisation support (defering all 
*init* functions to
`eval-after-load`), and multi repositories 
`ELPA` support.


## Version numbering

Version String are now inspired by how Emacs itself numbers its versions.
First is the major version number, then a dot, then the minor version
number.  The minor version number is 
`0` when still developping the next major
version.  So 
`3.0` is a developer release while 
`3.1` will be the next stable
release.

Please note that this versioning policy has been picked while backing
`1.2~dev`, so 
`1.0` was a 
*stable* release in fact.  Ah, history.
