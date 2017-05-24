+++
date = "2011-05-05T14:10:00.000000+02:00"
title = "Mailq modeline display"
tags = ["Emacs", "el-get", "modeline", "cssh", "mailq", "postfix"]
categories = ["Software Programming","Emacs Lisp"]
thumbnailImage = "/img/old/el-get.big.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/el-get.big.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/05/05-mailq-modeline-display",
           "/blog/2011/05/05-mailq-modeline-display.html"]
+++

If you've not been following along, you might have missed it: it appears to
me that even today, in 2011, mail systems work much better when setup the
old way.  Meaning with a local 
[MTA](http://en.wikipedia.org/wiki/Mail_Transfer_Agent) for outgoing mail.  With some niceties,
such as 
[sender dependent relayhost maps](http://tapoueh.org/articles/news/_Postfix_sender_dependent_relayhost_maps.html).

That's why I needed 
[M-x mailq](http://tapoueh.org/projects.html#sec21) to display the 
*mail queue* and have some easy
shortcuts in order to operate it (mainly 
`f runs the command
mailq-mode-flush`, but per site and per id delivery are useful too).

Now, I also happen to setup outgoing mail routes to walk through an 
*SSH
tunnel*, which thanks to both 
[~/.ssh/config](http://www.manpagez.com/man/5/ssh_config/) and 
[cssh](https://github.com/dimitri/cssh) (
`C-= runs the
command cssh-term-remote-open`, with completion) is a couple of
keystrokes away to start.  Well it still happens to me to forget about
starting it, which causes mails to hold in a queue until I realise it's not
delivered, which always take just about too long.

A solution I've been thinking about is to add a little flag in the 
[modeline](http://www.gnu.org/s/emacs/manual/html_node/elisp/Mode-Line-Format.html)
in my 
[gnus](http://www.gnus.org/) 
`*Group*` and 
`*Summary*` buffers.  The flag would show up as ✔ when
no mail is queued and waiting for me to open the tunnel, or ✘ as soon as the
queue is not empty.  Here's what it looks like here:

<center>
{{< image classes="fig50 fancybox dim-margin" src="/img/old/mailq-modeline-display.png" >}}
</center>

Well I'm pretty happy with the setup.  The flag is refreshed every minute,
and here's as an example how I did setup 
`mailq` in my 
[el-get-sources](https://github.com/dimitri/el-get) setup:

~~~
(:name mailq
		:after (lambda () (mailq-modeline-display)))
~~~


I'm not sure how many of you dear readers are using a local MTA to deliver
your mails, but well, the ones who do (or consider doing so) might even find
this article useful!
