+++
title = "Sustainable Open Source Development"
date = "2018-02-20T21:08:38+01:00"
tags = ["PostgreSQL","pgloader","Sustainable","Open Source","Funding"]
categories = ["PostgreSQL","pgloader"]
coverImage = "/img/Pontdugard.jpg"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/recycle-icon.jpg"
thumbnailImagePosition = "left"

+++

Current trend in software deployments is to rely on open source software for
entire production stacks. You can find open source software in the core
technical stacks of every startup out there, I'm told. If you're using Cloud
based offerings, most of Cloud providers are running [Free/Libre Open Source
Software](https://www.gnu.org/philosophy/floss-and-foss.en.html) as their
foundation.

This article is a deep dive into the economic models behind successful open
source projects and communities, and how as a professional, enterprise grade
user, you depend on the long-term sustainability of all the open source
projects you're using. And because you depend on the projects you're using
to be successful, how to contribute and guarantee their success.

<!--more-->

{{< alert info >}}

If you're reading this article as a Software Hacker, typically as an
Individual Contributor in your enterprise, that's good, I think you'll be
interested by the contents. That said, make sure to send it over to those
people who buy your software licenses and other stuff that you need to be
able to work, because I want **them** to read this piece. Them who may spend
company money can help making Open Source development sustainable…

{{< /alert >}}
{{< alert success >}}

If you're reading this article as someone who is used to buy software
licenses for your developers to be able to work, either for interactive
editors or project management and online code repositories, please read this
article. **TL;DR**: if you're using [pgloader](https://pgloader.io),
contribute either time, money, or both to the project. Scroll to [The
pgloader Moral License](#the-pgloader-moral-license) at the end of this
article to learn how easily you can do that.

{{< /alert >}}

<hr />

{{< figure class="right" src="/img/gnu.160.png" >}}

You can choose to deploy your web application or services using
[django](https://www.djangoproject.com), [rails](http://rubyonrails.org),
[nodejs](https://nodejs.org/en/), some [Go](https://golang.org) based
framework such as [Iris](https://iris-go.com) or
[Revel](https://revel.github.io). Or [PHP](http://php.net) frameworks
[Symfony](http://symfony.com) or the new kid in the block
[Laravel](https://laravel.com). Or you might roll out Java™ based
technologies such as [Tomcat](http://tomcat.apache.org),
[JBoss](http://www.jboss.org) or
[GlassFish](https://javaee.github.io/glassfish/). Or you might prefer to use
a [.Net](https://www.microsoft.com/net/) environment.

There's something common in the technologies listed above, and many more
you're most certainly relying on: all of them are Open Source Software. It
means you can access the source code, run it locally yourself, play with it
and hack it away either to learn how it's done or even to fix bugs… or to
add new features. Then you're welcome to contribute your improvements to the
community, so that everyone can benefit from your awesome work!

<!--toc-->

## Why is Everybody Using Open Source?

Open Source Software has not always been the first choice of enterprise as
pillars of their production infrastructure. It used to be that company
decisions would favor using proprietary software, closed source, with
expensive maintenance contracts. What it meant is that every single customer
of a given vendor would separately pay for the same bug fixes, unknowingly.

There was such a time when Open Source Software quality was not there yet,
so that you could not rely on them for running your production. And before
that, maybe the software wasn't available at all, or for your particular
hardware choice… yeah it used to be that people had a choice to make amongst
more than just 3 hardware architecture…

So how did we arrive at the current situation, where it doesn't make sense
anymore to use proprietary and closed source software for most of the
things? Investment is the answer. Individual hackers and contributors have
investing humongous amounts of time into their little Open Source Software
alternatives, up to a point where our Free Software catalog is running the
world.

Who paid for this initial investment? Public research labs and facilities
such as those running with universities, individuals on their _free time_,
and then slowly some private companies who allowed some of their staff to
use their paid time to contribute, up until when it has become obvious that
the best solution both in terms of economics and in terms of quality would
be to contribute to the common goods, our beloved Open Source Software.

Many people are still investing their own _free time_, by the way, and
that's how we now have those advanced core technologies to rely our products
on. **Standing on the shoulders of giants**, as they say.

## Sustainable Open Source

More and more Open Source Software contributors are paid to improve Open
Source Software, and most of them to that in the open and actually
contribute to the software that is available to everybody. That's the right
way to do it when handling [Common
Good](https://en.wikipedia.org/wiki/Common_good).

That said, with the rise of Cloud based computing, more and more people are
also paid to improve Open Source Software without contributing their
improvements back to the communities. That can been seen as a loop hole in
the license terms, or a plain refusal to understand the rules. Most
importantly, it's all about taking for yourself what was freely offered to
be shared with you.

Open Source Software nowadays is mainly maintained in one of those possible
organisations:

  - Open Source Editors

    Some Open Source Software are now developed, maintained and released by
    commercial editors for whom it makes sense to offer their solution under
    FLOSS licencing.

  - Independant Communities
  
    Most Open Source Software projects don't have an enterprise editing
    them. Instead a community of individual contributors are working on the
    project, either as part as their daily job or most often, on their _free
    time_, because they like the project.
  
  - Single Person Projects
  
    And a huge number of smaller software projects are still taken care of
    by a single individual who is devoted to his project, for some reason or
    another.
    
For some projects, the sustainability isn't much of a concern, because of a
very small and motivated user group, who will either find another solution
or learn how to maintain the software if needed. That's mainly for software
you would use as a hobby.

For other projects though, I'd say as soon as when the project is used in an
enterprise setting, then ensuring a mid to long term sustainability is
crucial to the project users. Because now an enterprise business depend on
the availability of some external software.

As an enterprise using Open Source Software, you should learn enough about
the project's organisation to assess if it's reliable for you to use it.
When it's not, then you have two ways to make the software reliable for you:

  - Contribute time to the software
  
    That's as easy as allowing proper paid time to your staff to contribute
    to the Open Source Software you're using, in particular when the project
    does not have the backing of a professional editor behind it.
    
  - Contribute money to the software
  
    If you can't have your staff devote time to learn the project's code and
    how to contribute to it, then you need to make it so that the project is
    in a good position to do it themselves when you need them to. That means
    contributing money to the project so that they are able to pay for other
    hacker's time — after all time is money, and you're not willing to put
    time on the table, so put money on it, to much the same effect, right?

## Open Source Editors Business Models

{{< figure class="right"
             src="/img/Modèles-économiques-200x320.jpg"
            link="https://systematic-paris-region.org/wp-content/uploads/2017/07/LivretBleu_ModelesEconomiques_GT-LogicielLibre_Systematic.pdf" >}}

When an Open Source project is edited by a professional editor, then we can
observe a short list of business models that are often used. The French free
software work group for research and development investments
[Systematic](https://systematic-paris-region.org/fr/groupe-thematique-logiciel-libre/)
has published a paper about those business models.

The most common open source business models for software editors are:

  - Foundations
  
    Foundations typically involve several companies working together on the
    same software, in order to lower their R&D costs. They're funded by a
    group of enterprises who would each need to maintain the same source
    base anyway.
  
  - Double Licencing
  
    The software is available either as under FLOSS license terms, using
    either the GPL or the [Affero GPL
    license](https://www.gnu.org/licenses/why-affero-gpl.en.html), or under
    a prorietary license for those users who don't want to publish their own
    software as a FLOSS project. Revenues from selling proprietary licenses
    allows to invest in the FLOSS version of the software.

  - Open Core
  
    The core of a product is Open Source and available under FLOSS licencing
    terms, and the editor also builds a proprietary product on-top of the
    FLOSS offering, with an additional feature set. Revenues from selling
    the extended product allows to invest in the basic FLOSS version.
    
  - Cloud
  
    The editor maintains an Open Source Software and also offers a managed
    [SaaS](https://en.wikipedia.org/wiki/Software_as_a_service) solution.
    Again, a part of the revenues from the offer is invested back into the
    Open Source Software maintenance and improvements.
    
  - Professional Open Source Services
  
    The editor contributes to an Open Source Software and then sells
    professional services to its users, such as _follow the sun_ 24/7
    support that includes upstream bug fixes, specific software builds and
    maintenance, on-demand feature development of either extensions or new
    features.

{{< figure class="right"
             src="/img/postgresql-200.jpg"
            link="https://www.postgresql.org" >}}

[PostgreSQL](https://www.postgresql.org) itself is very lucky in that
several companies are contributing to the project while entertaining at
least one of the previous business models. That's how PostgreSQL came to be
the world's most advanced open source database and continues to strive at a
very high pace, releasing every year a new major version packed with
improvements, fixes, and new features.

## Single Person Projects Business Models

So with people getting paid to contribute to Open Source Software, we could
think that the times or yore are over now, and all is good in our world.

While some highly visible Open Source Projects have the opportunity to
attract direct contributions from smart corporations who understood the
economics behind it, lots of smaller projects, often one-person projects,
are not so lucky, and are actually dependant on the willingness of a single
contributor that handles the project on their _free time_.

As an enterprise user of such a project, you should wonder how reliable the
situation is, and how to make it actually reliable. As mentioned before,
it's all about you being able to contribute to the project either time,
money, or both.

When developing and maintaining an Open Source Software as a single
individual, how do you make it so that it's possible for you to sustain your
activity?

The least you have to do is make it so that you can pay the bills and
support your family, and maybe it woud be best for the project to benefit
from its maintainer to be able to spend more than just _free time_ on fixing
bugs and improving the software.

Now, how to make that happen is still a gray area, and I think we need to
find better answers than are currently broadly available. I like _The
Varnish Moral License_ idea a lot, and another widely adopted solution
consists of using a _crowd funding_ solution for sponsoring a specific
feature set.

## The Varnish Moral License

[The Varnish Moral License](http://phk.freebsd.dk/VML/) idea from
[Poul-Henning Kamp](http://phk.freebsd.dk/) is quite simple. In his own
words, here's how it works:

>   - Happy varnish users tell me that they want a Varnish Moral License
>   - I send them an invoice from my company
>   - They pay the invoice
>   - I develop Varnish

> A Moral License to make any kind of big or small money running Varnish on
> your website, without feeling the least bit bad about the poor sucker who
> created the software, his two kids, his old cat etc etc.

> On a more concrete level, the Varnish project gets more of my time and
> attention which I use to improve the tool which makes your website work so
> well.

> Somebody started the rumour that the name was chosen to sneak this expense
> into your IT departments budget, along with other software licenses,
> rather than put it under your marketing department, where all sponsorships
> properly belong and where it would compete with the local soccer team.

> That rumour is very possibly true. I might even have started it myself.
> Right here even.

## Let's Talk about pgLoader now

As you might know already, I am personnaly involved in a Single Person Open
Source Software Project: [pgloader](https://pgloader.io). This project
implements loading data into PostgreSQL in a fully automated way, to enable
the following two use cases:

  - Load data from files with automatic error handling.
  
  - Implement [Continuous
    Migration](https://pgloader.io/blog/continuous-migration/) from your
    current database to PostgreSQL.

As you can imagine, the [pgloader](https://pgloader.io) project is mainly
used in enterprise setups, and so we now have a bunch of companies who
depend on my abilities to single-handedly maintain, improve and release the
software.

{{< figure class="right"
             src="/img/static-animated-citus.200.png"
            link="https://www.citusdata.com" >}}

On a personal level, I'm lucky enough to get paid to work on an awesome
PostgreSQL Extension — check it out at [Citus
Data](https://www.citusdata.com), the Worry-Free Postgres for SaaS.

This is a classic Open Source paradox: my contributions to PostgreSQL allow
me to have a very good job at a great company, and I also have a bunch of
projects I have started along the years and that I maintain on my own. Some
of those projects certainly deserve more attention that what I can offer on
my _free time_. Those projects include [pgloader](https://pgloader.io) of
course, and also the PostgreSQL [prefix](https://github.com/dimitri/prefix)
extension that I know is used in production in several telecom companies, or
the [PostgreSQL Extension White
Listing](https://github.com/dimitri/pgextwlist) that is used in production
at several PostgreSQL SaaS providers, and which should now be deprecated by
the more ambitious project
[pginstall](https://github.com/dimitri/pginstall)… which never took off
because I couldn't make it easy enough to deploy, basically.

What I would like to achieve is an organisation where enterprises that are
using [pgloader](https://pgloader.io) can help sustain the project in the
mid to long term. Contributing time to the project is already easy, but
seldom happens. After all pgloader is not an infrastructure level project,
it's more like a background job that you script around then mostly forget
about. So if the product works well enough to solve your immediate problem,
that's good, and now you're already doing something else.

And if you don't have to contribute to pgloader in order to successfully use
it, then why would you bother investing time on the project? While that's
understandable, it leaves the whole idea of the project being sustainable
unanswered.

That's why I've been setting up funding solutions for the project. The idea
is that I could then hire fellow Open Source Hackers to work on the project
with me and improve it, fix bugs, develop new features. We have a long list
of improvements to make to the project, and I often talk with people from
different companies who would like to see them happening.

Of course, I don't know anyone who would hack Oracle™ support into
[pgloader](https://pgloader.io) on their own _free time_ so that some big
companies can now easily migrate their legacy systems to PostgreSQL,
cancelling huge amounts of license fees in the making. That said, I know
several Common Lisp hackers who are for hire and would be happy to be paid
to contribute to enterprise grade Open Source Software solutions…

## The pgLoader Moral License

As I really like [The Varnish Moral License](http://phk.freebsd.dk/VML/)
idea, this funding option is of course available to
[pgloader](https://pgloader.io). If you're an enterprise using
[pgloader](https://pgloader.io), consider contributing to the project either
time or money, or both:

<script src="https://gumroad.com/js/gumroad-embed.js"></script>
<div class="gumroad-product-embed" data-gumroad-product-id="pgloader"><a href="https://gumroad.com/l/pgloader">Loading...</a></div>

## The pgloader Patrons Membership

{{< figure class="right"
             src="/img/pgloader.320.jpg"
            link="https://pgloader.io" >}}
             
The pgloader project is fully Open Source and released under [The PostgreSQL
License](https://www.postgresql.org/about/license/) so that anyone can
easily contribute to the project. The whole project management (issue
tracking, feature proposals, etc) happens on the [pgloader github
page](https://github.com/dimitri/pgloader), in public.

The pgloader Moral License is a one-off payment — that you can do as many
times as you like. That's a good way to show support for the project and
allow specific feature development, but maybe that's not enough for me to
hire Common Lisp hackers and improve the many little things that need strong
focus and attention in the project.

That's why I've also setup a recurring payment solution, the
[pgloader](https://pgloader.io) Patrons Membership, that allows a monthly to
yearly commitment to sustain the project improvements in the long term:

<div class="gumroad-product-embed" data-gumroad-product-id="CjXn"><a href="https://gumroad.com/l/CjXn">Loading...</a></div>

## Conclusion

In this very long article we've been talking about how to sustain Open
Source Software development both at the editor level and at the individual
level. While several business models are available for both situations, it's
still not that easy in 2018 for enterprise users of Open Source Software to
ensure that the projects they depend on are sustainable.

We could wait until people like [GitHub](https://github.com) or
[GitLab](https://about.gitlab.com) integrate Open Source project funding
options, or we can try to solve the problem at our individual scope, one
project at a time, like [Poul-Henning Kamp](http://phk.freebsd.dk/) is doing
with [The Varnish Moral License](http://phk.freebsd.dk/VML/).

Let's build a bridge between companies using Open Source Software and
individual contributors using their own personal time to maintain and
improve innovative solutions!
