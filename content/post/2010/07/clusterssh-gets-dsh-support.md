+++
date = "2010-07-23T22:20:00.000000+02:00"
title = "ClusterSSH gets dsh support"
tags = ["Emacs", "el-get", "cssh"]
categories = ["Software Programming","Emacs Lisp"]
thumbnailImage = "/img/old/el-get.big.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/el-get.big.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/07/23-clusterssh-gets-dsh-support",
           "/blog/2010/07/23-clusterssh-gets-dsh-support.html"]
+++

If you don't know about 
[ClusterSSH](cssh.html), it's a project that builds on 
`M-x term`
and 
`ssh` to offer a nice and simple way to open remote terminals. It's
available in 
[ELPA](http://tromey.com/elpa/index.html) and developed at 
[github cssh](http://github.com/dimitri/cssh) repository.

The default binding is 
`C-=` and asks for the name of the server
to connect to, in the 
*minibuffer*, with completion. The host list used for
the completion comes from 
`tramp` and is pretty complete, all the more if
you've setup 
`~/.ssh/config` with 
`HashKnownHosts no`.

So the usual way to use 
`cssh.el` would be to just open a single remote
connection at a time. But of course you can open as many as you like, and
you get them all in a mosaic of 
`term` in your emacs frame, with an input
window at the bottom to control them all. There were two ways to get there,
either opening all remote hosts whose name is matching a given regexp, that
would be using 
`C-M-=` or getting to 
`IBuffer` and marking there
the existing remote 
`terms` you want to control all at once then use
`C-=`.

Well I've just added another mode of operation by supporting 
*enhanced* 
[dsh](http://www.netfort.gr.jp/~dancer/software/dsh.html.en)
group files. In such files, you're supposed to have a remote host name per
line and that's it. We've added support for line containing 
`@group` kind of
lines so that you can 
*include* another group easily. To use the facility,
either open your 
`~/.dsh/group` directory in 
`dired` and type 
`C-=`
when on the right line, or simply use the global 
`C-=` you
already know and love. Then, type 
`@` and complete to any existing group found
in your 
`cssh-dsh-path` (it defaults to the right places, so chances are you
will never have to edit this one). And that's it, 
[Emacs](http://www.gnu.org/software/emacs/) will open one 
`term`
per remote host you have in the 
`dsh` group you just picked. With a 
`*cssh*`
controler window, too.

Coming next, how I solved my 
`init.el` dependancies burden thanks to 
`el-get`!
