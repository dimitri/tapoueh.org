+++
date = "2010-09-23T14:30:00.000000+02:00"
title = "Postfix sender_dependent_relayhost_maps"
tags = ["mailq", "postfix"]
categories = ["Software Programming","Emacs Lisp"]
thumbnailImage = "/img/old/article2.gif"
thumbnailImagePosition = "left"
coverImage = "/img/old/article2.gif"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/09/23-postfix-sender_dependent_relayhost_maps",
           "/blog/2010/09/23-postfix-sender_dependent_relayhost_maps.html"]
+++

The previous article about 
[M-x mailq](http://tapoueh.org/articles/news/_Scratch_that_itch:_M-x_mailq.html) has raised several mails asking me
details about the 
[Postfix](http://www.postfix.com/) setup I'm talking about. The problem we're trying
to solve is having a local 
`MTA` to send mails, so that any old-style Unix
tool just works, instead of only the 
`MUA` you've spent time setting up.

Postfix makes it possible to do that quite easily, but it gets a little more
involved if you have more than one 
*relayhost* that you want to use depending
on your current 
*From* address. Think personal email against work email, or
avoiding your 
`ISP` network when sending your private mails, 
*hoping* directly
on a server you own or trust.

So how do you do just that? Let's see the relevant parts of 
`main.cf`.

~~~
relayhost = your.default.relay.host.here
relay_domains = domain.org, work-domain.com, other-domain.info
smtp_sender_dependent_authentication = yes
sender_dependent_relayhost_maps = hash:/etc/postfix/relaymap
~~~


The 
`relaymap` looks like this:

~~~
# comments
user@domain.org         mail.domain.org
local@work-domain.com   smtp.work-domain.com
# that requires a local tunnel started with ssh, see ~/.ssh/config
me@other-domain.info    [127.0.0.1]:10025
~~~


You need to use 
[postmap](http://www.postfix.org/postmap.1.html) on this file before to reload or restart your local
instance of Postfix.

Also, you should want to crypt your communication to your preferred relay
host, using 
`TLS` goes like this:

~~~
smtp_sasl_auth_enable=yes
smtp_sasl_password_maps=hash:/etc/postfix/sasl-passwords
smtp_sasl_mechanism_filter = digest-md5
smtp_sasl_security_options = noanonymous
smtp_sasl_mechanism_filter = login, plain
smtp_sasl_type = cyrus

smtp_tls_session_cache_database = btree:${queue_directory}/smtp_scache
smtp_tls_loglevel = 2
smtp_use_tls = yes
smtp_tls_security_level = may
~~~


The password file will need to get parsed by 
`postmap` too, and would better
be set with limited read access, and looks like this:

~~~
mail.domain.org        user@domain.org:password
smtp.work-domain.com   local@work-domain.com:h4ckm3
[127.0.0.1]:10025      me@other-domain.info:guess
~~~


Hope this help you get started, at least that's a document I would have
enjoyed reading when I first started to setup my local relaying 
`MTA`.

Oh, and now that you have this, I hope you will enjoy my 
`M-x mailq` tool for
occasions when you're wondering why you're not receiving an answer back yet,
then start the ssh tunnel…
