+++
date = "2010-07-22T09:30:00.000000+02:00"
title = "Emacs and PostgreSQL"
tags = ["PostgreSQL", "Emacs", "debian", "plpgsql", "pgsql-linum-format"]
categories = ["debian"]
thumbnailImage = "/img/old/debian-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/debian-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/07/22-emacs-and-postgresql",
           "/blog/2010/07/22-emacs-and-postgresql.html"]
+++

Those are my two all times favorite Open Source Software. Or 
[Free Software](http://www.gnu.org/philosophy/free-sw.html)
in the 
[GNU](http://www.gnu.org/) sense of the world, as both the 
*BSD* and the 
*GPL* are labeled free
there. Even if I prefer the 
[The Debian Free Software Guidelines](http://www.debian.org/social_contract) as a global
definition and the 
[WTFPL](http://sam.zoy.org/wtfpl/) license. But that's a digression.

I think that 
[Emacs](http://www.gnu.org/software/emacs/) and 
[PostgreSQL](http://www.postgresql.org/) do share a lot in common. I'd begin with
the documentation, which quality is amazing for both projects. Then of
course the extensibility with 
[Emacs Lisp](http://www.gnu.org/software/emacs/emacs-lisp-intro/html_node/Preface.html#Preface) on the one hand and
[catalog-driven operations](http://www.postgresql.org/docs/8.4/static/extend.html) on the other hand. Whether you're extending Emacs
or PostgreSQL you'll find that it's pretty easy to tweak the system 
*while
it's running*. The other comparison points are less important, like the fact
the both the systems get about the same uptime on my laptop (currently 
*13
days, 23 hours, 57 minutes, 10 seconds*).

So of course I'm using 
*Emacs* to edit 
*PostgreSQL* 
`.sql` files, including stored
procedures. And it so happens that 
[line numbering in plpgsql](http://archives.postgresql.org/pgsql-hackers/2010-07/msg01067.php) is not as
straightforward as one would naively think, to the point that we'd like to
have better tool support there. So I've extended Emacs 
[linum-mode minor mode](http://www.gnu.org/software/emacs/manual/html_node/emacs/Minor-Modes.html)
to also display the line numbers as computed per PostgreSQL, and here's what
it looks like:


{{< image classes="fig50 center fancybox dim-margin" src="/img/old/emacs-pgsql-line-numbers.png" >}}


Now, here's also the source code, 
[pgsql-linum-format](https://github.com/dimitri/pgsql-linum-format). Hope you'll enjoy!
