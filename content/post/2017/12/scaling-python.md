+++
title = "Scaling Python released"
date = "2017-12-05T18:10:00+01:00"
tags = ["PostgreSQL","Python","book"]
categories = ["PostgreSQL","Python"]
coverImage = "/img/scapy-title.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/icon-culture-book2.png"
thumbnailImagePosition = "left"

+++

Today I am very pleased to announce the release of the book [Scaling
Python](https://scaling-python.com) from my good friend [Julien
Danjou](https://julien.danjou.info/)!

As Julien says, **Python applications can handle millions of requests.**
Well, we know here that it's easier on them when they are using PostgreSQL
of course!

<!--more-->

# An Interview with Julien Danjou

  - Julien, this is your second book about write code in Python. Who are the
    target audience of both your books?

    > I would say that they both share the same audience: Python developers
    > that are familiar with the language but want to become more productive
    > and build better applications with it. The Hacker's Guide to Python
    > covers general Python best practice, productivity tip, insight on how
    > CPython works internally and presents the state of the art in many
    > different topics.
    
    > Scaling Python, my new opus, is targeting developers that want their
    > code to be ready for tomorrow. Writing applications that can handle
    > thousands of requests in a snap does not sound that easy. The book
    > breaks down that large problem in simpler ones and provides solutions
    > to solve them.
    
  - Python application and backends are often found using a PostgreSQL
    database server behind them, for the data processing parts of the
    application. You told me your book doesn't address this part. Is it
    because PostgreSQL just works?

    > Scaling Python does not cover PostgreSQL specifically because it is
    > not a problem. Most bottlenecks in (Python) applications are not due
    > to the database but caused by the application not being designed in
    > any proper way. There are good practices and design that would work
    > well when writing Python application where PostgreSQL handles the data
    > processing: built it stateless, distribute the workload on multiple
    > CPU/nodes, use caching properly, etc. The subjects the book cover fits
    > well here.

  - Is there anything you might want to way to PostgreSQL users, application
    developers and DBAs about using Python in general, or your current book,
    Scaling Python, in particular?

    > I think PostgreSQL and Python are good friends. You don't (often) use
    > PostgreSQL alone – you need an application in front of it to insert
    > and retrieve the data. Python is a salutary language in this regard
    > which you can both leverage in your application and with PL/Python!
    > Using Python the proper way and designing code that performs at large
    > scale is a challenge – so if your development team needs a hand, I'd
    > suggest taking a look at Scaling Python! :)

# Scaling Python, the packages

The book comes in a set of editions tailored to different needs, so that you
can pick the one that makes sense for you. Also, Julien is kindly offering
15% off for my readers during the next 48 hours for any of the edition of
the book. Just use the *PYTHON-LOVES-POSTGRESQL* coupon code in any of the
following package:

## Scaling Python, Enterprise Edition

<strike>$199</strike> [$169](https://gumroad.com/a/1014150259/trPYm) with
coupon code *PYTHON-LOVES-POSTGRESQL*

{{< figure src="/img/scaling-python-package-enterprise.png"
          link="https://gumroad.com/a/1014150259/trPYm" >}}

  - The ebook in PDF, HTML, MOBI and EPUB formats
  - 7 experts interviews
  - Future editions of the ebook
  - All code examples ready to run
  - Docker image with the complete environment to run all the book examples
  - Web interface to run all the examples as notebook – easy editing and toying
  - License to share with 10 members of your team

<div class="button buy">
  <a href="https://gumroad.com/a/1014150259/trPYm">Buy The Enterprise Edition</a>
</div>

<hr>

## Scaling Python, Standard Edition

<strike>$89</strike> [$75](https://gumroad.com/a/1014150259/ZUHTn) with
coupon code *PYTHON-LOVES-POSTGRESQL*

{{< figure src="/img/scaling-python-package-standard.png"
          link="https://gumroad.com/a/1014150259/ZUHTn" >}}

  - The ebook in PDF, HTML, MOBI and EPUB formats
  - 7 experts interviews
  - Future editions of the ebook
  - All code examples ready to run
  - Docker image with the complete environment to run all the book examples

<div class="button buy">
  <a href="https://gumroad.com/a/1014150259/ZUHTn">Buy The Standard Edition</a>
</div>

<hr>

## Scaling Python, Old-School Edition

<strike>$49</strike> [$43](http://www.lulu.com/commerce/index.php?fBuyContent=21930016)

{{< figure src="/img/scaling-python-package-oldschool.320.png"
         class="right"
          link="http://www.lulu.com/commerce/index.php?fBuyContent=21930016" >}}

 - The book printed in paperback format, delivered to your door
 - The ebook in PDF, HTML, MOBI and EPUB formats
 - 7 experts interviews
   
> This purchase will go through Lulu, who do not distribute the electronic
> format. Forward your Lulu receipt to Julien to get the electronic version.

<div class="button buy">
  <a href="http://www.lulu.com/commerce/index.php?fBuyContent=21930016">Buy The Old-School Edition</a>
</div>

<hr>

## Scaling Python, Starter Edition

<strike>$39</strike> [$33](https://gumroad.com/a/1014150259/ZUHTn) with
coupon code *PYTHON-LOVES-POSTGRESQL*

{{< figure src="/img/the-hacker-guide-to-scaling-python.png"
         class="right"
          link="https://gumroad.com/a/1014150259/zoEUp" >}}

The Hacker's Guide to Scaling Python ebook in PDF, HTML, EPUB and MOBI
formats.

<div class="button buy">
  <a href="https://gumroad.com/a/1014150259/zoEUp">Buy The Starter Edition</a>
</div>

<hr>

If you have any question, feel free to reach
[Julien](mailto:julien@danjou.info) directly and he will be happy to reply.
Or write in the comment section below!

And don't worry: if the book is not what you expect it to be and has no
value to you, then just say so and Julien will refund you, no questions
asked.
