+++
title = "Scheduled Data Processing: How to use cron?"
date = "2018-08-01T11:24:25+02:00"
tags = ["PostgreSQL","YeSQL","Concurrency","Batch","Cron","Opinion"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/scheduler.png"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/crontab.jpg"
thumbnailImagePosition = "left"

+++

A previous article in the [PostgreSQL Concurrency](/tags/concurrency) series
covered how to manage concurrent retweets in an efficient way: in [Computing
and Caching](/blog/2018/07/computing-and-caching/), we learnt how to
maintain a cache right in your PostgreSQL database, using MATERIALIZED
VIEWS. We also had a look at how to take care of [Batch Updates and
Concurrency](/blog/2018/07/batch-updates-and-concurrency/).

While in the first case we are providing a solution to a technical problem
where we want to solve performance issues while keeping the same semantics,
in the second case we are actually implementing a part of the application's
[Business Logic](/blog/2017/06/sql-and-business-logic/) as a scheduled job.

Today's article shows a modern technique to handle the scheduling of those
business oriented activities that are not tied to any user activity. When
thinking about it this way, you certainly don't want to implement the
backbone of your business logic in a *shell script* that's directly
maintained in the production environment, do you?

<!--more-->
<!--toc-->

## Scheduling Data Processing

Data processing is most often triggered by user actions, either your
end-user or internal users: back-office, admin superusers, or support users
maybe. In some cases though, it may happen that some data processing needs
to happen on its own, following a schedule rather than being triggered by
direct user activity on your product.

As we saw in [Computing and Caching](/blog/2018/07/computing-and-caching/),
the use case might be a technical implementation detail. It might also be
user oriented, like daily dashboard metrics computating, account
consolidating, user-defined alerting schedule (daily or weekly summaries,
etc), or many other use cases really.

When the use case for scheduling data processing is business oriented, then
the usual questions about [SQL and Business
Logic](/blog/2017/06/sql-and-business-logic/) are back in play again. If
we're implementing daily activity reporting by email to our users, of course
the email contents are going to be driven by business logic. The numbers
you're sending are the result of the business rules used to compute them.

So we have to trigger data processing at fixed moments in time rather than
triggering it with some user activity. The classic way to implement that is
to use the venerable [crontab](https://crontab.guru) facility from Unix
systems.

In classic Unix phylosophy, [Cron](https://en.wikipedia.org/wiki/Cron) is
great at just one thing: starting a user defined command at a specific time
specification, which can be… almost anything. Once in a blue moon. Every
other Thursday at 6pm. Every morning. At boot time. About any point in a
calendar can be specified as a cron schedule expression, and that includes
specifications of recurring events.
  
## How NOT to use cron?

The classic approach to cron jobs could be qualified as surprising, if you
want to be nice. It's actually wrong in that it solves none of the
interesting problems you have when using cron.

A very classic cron usage looks like the following:

~~~ cron
0 22 * * 1-5 /usr/local/bin/my-script.sh
~~~

The _cron_ unix service is then going to call `/usr/local/bin/my-script.sh`
at 22:00 on every day-of-week from Monday through Friday. All that's left is
for the bash script to implement the data processing facility we need.

An improvement to the bash script that is often found in production
environments consists of using a reasonable programming language to write
your cron command in. By that I mean a programming language that makes it
**easy** to handle **unforeseen situations**: it usually requires a kind of
a [condition
system](http://www.gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html)
or at least the ability to handle errors and exceptions.

Your bash script might being with a call to
[trap](https://www.shellscript.sh/trap.html) if you're an advanced enough
user, unfortunately that's pretty rare and only gets you so far.

Even when using a reasonable programming language, the needs for a
processing job that is scheduled in the background are a little more
involved than that.

## Code Management and Deployment

Another important aspect in that usage of cron is the code management. Often
enough cron commands are bash scripts. Sometimes cron commands are properly
implemented with the same programming language as the rest of the user
facing application, using the same business rules and code.

In both cases, it's important to note that the code should be managed in a
proper code repository, versionned, and deployed with the same process and
quality as applied to the rest of the code.

In many places, even when people are serious about their deployments, I've
seen cron jobs falling in between the devs and ops team, noone being the
owner of them, and thus no clear procedure would be in place for maintaining
those scripts.

## The Real Needs of a Scheduled Data Processing Job

So, why don't I just run my cron jobs in bash, you're asking? Well because
here's a list of things that I want to be able to **easily** know, verify
and act on when dealing with a data processing job scheduled in the
background:

  - When did it run? for how long?
  
    I know when I said it should start, that's the cron specification. Of
    course I might have been wrong when writting that, so I want to be able
    to debug it easily. Also, we might have a system that prevents the same
    processing job to run more than once on the same server, usually that
    would be achieved with a *lock file*.
    
    A very classic gotcha is to forget about how long the job is taking when
    computing its frequency. Say we want to run a job every 5 minutes, so we
    have a cron entry like the following:
    
    ~~~ cron
    */5 * * * * /usr/local/bin/my-script.sh
    ~~~
    
    Then if we have a *lockfile* (we usually should) and the processing runs
    for more than five minutes, we're going to skip scheduled runs. If the
    aim is to guarantee that the data is at least as fresh as 5 minutes ago,
    that's a problem and I want to know about it.
    
    Sure, I can send messages to the logs and have some automated processing
    for that, and most cron/bash users probably use
    [logger](http://man7.org/linux/man-pages/man1/logger.1.html) for that
    already, with a complex log processing setup behind it (_logstash_ or
    _ELK_ are some of the popular stacks for handling logs).
  
  - What did the job had to do, what did it do?
  
    Now, as soon as the job is implementing some business logic, I want to
    have some metrics about what the data processing job did, in terms of my
    business logic, and I want those metrics displayed at a place where our
    internal teams (accounting, finance, marketing, etc) can see them, and
    expressed in a way that makes sense for them.
    
  - How to check if the processing was correct?
  
    Depending on what the processing is all about, it might be worthwile to
    have both a summary view of what the job did (how many items it
    processed, to which cumulated amount of *value*, etc), and sometimes a
    detailed view where you can dive in any of the processed items and see
    what the processing was all about.
    
    If you're using PostgreSQL, then having a generic before/after diff is
    easy enough to implement thanks to the
    [hstore](https://www.postgresql.org/docs/current/static/hstore.html)
    extension, as detailed in my article [Auditing Changes with
    Hstore](/blog/2013/08/auditing-changes-with-hstore/).
  
  - Was there any failure? how to review and process the failures?

    Now if some items failed to be processed, our job might be smart enough
    to mark them as failed and continue with other tasks. In that case,
    someone has to review the failed items and do something about them. 
    
    Or maybe it's better to fail early and just retry that same job over and
    over again, assuming that something will change that allow the
    processing to succeed. An upgrade of the data processing utility, for
    instance.
    
    It might be that your application is now producing new kinds of events
    that you don't know how to handle yet. Or just a plain bug that you're
    lucky enough is failing the processing rather than silently producing
    the wrong results…

  - If a job is still running, can I stop it? restart it? cancel it?
    reschedule it?
  
    And that one is pretty important. Say you have a nightly job to run and
    for some reasons it failed to run yesternight. Maybe because you
    scheduled it at 2:30am but thanks to daylight saving this time
    specification didn't happen yesterday. Yes, that's a classic. So what
    now? Can you run the cron job again easily in production?
    
    Another use case here is that famous _infinite loop_ that only happens
    with the exact right setup and data combo, and so you didn't think of
    guarding against the case in your code because it won't happen. Well, it
    did. It's actually happening now. How easy is it for you to take control
    of your background job?

Those are some pretty basic questions here, and if the data processing jobs
that you schedule are important for your business, then I'm sure you want to
be able to answer all of them.

## Not all cron jobs are the same

It must also be said that we have two very different cases to think about
when discussing cron jobs:

  1. Technical cron jobs, like archiving, backups and restore, and other
     commands related to the operational architecture.
     
     In that case, most probably the team that owns the cron commands is an
     ops team, and the tooling they use day in and day out is going to be
     well suited for making all the previous requirements *easy enough* to
     implement for them.
     
     After all, _ssh_, _ps_, _kill_, _rm -f_ and all are proper tooling if
     you're an ops guy debugging something unexpected in production, in many
     cases. And when you prefer more automation around your ops process, I'm
     sure you know how to include cron jobs debugging in your tooling.
     
  2. Business Logic that needs to be scheduled at known time specification.
  
     That's what this blog article is all about. Because now, you need to
     write application code that happens to run when specified by your
     crontab, or with some other scheduler.
     
     Now debugging the job isn't going to be done by the ops team, because
     they're not tasked with understanding the fine points of the business
     rules, and most of the bugs you'll want to address here aren't going to
     be related to your production's architecture, rather to those business
     rules.
     
     That's when the tooling used in production should be *easy enough* for
     an application developer perspective. And in most places, having to
     fiddle around interactively on production systems to fix a business
     rule implementation bug isn't the best way to ensure quality.
     
We need a solution that can enable a developer team to manage the scheduled
events properly, and without requiring fiddling interactively at a
production's system shell.
  
## Proper Usage of Cron

The idea that I want to push forward in this article is the following: when
you need to schedule data processing, write a web service that implements
your data processing, with full manual controls over it, a dashboard and
logs. Some of the things your service must implement are:

  - A dashboard to know what's been done when, and what's currently
    happening.
  
  - An easy way to read the logs from your background activities.
  
  - Some ways to see the failed processing.
  
  - Maybe a way to manually handle the failures, or re-inject them into the
    pipeline after having (maybe?) fixed exposed bugs.
    
  - A control to start, stop, resume, and restart current jobs.
  
  - An API that allows to start a new job non-interactively, and that we are
    going to use in our crontab entry.

One way to get started is to implement those features on top of a very basic
data processing function: a function that fails with a _Not Yet Implemented_
error. Then you can debug your background job by adding the missing rules
and processing, and use the interactive controls to restart job processing
from the application, and see the dashboards getting updated with success
and failures, and metrics that make sense for whatever it is you are
implementing.

When you have it working, it's time to deploy the service and implement the
scheduling parts. As your background data processing jobs are now
implemented as web services, your organisation probably already knows how to
deploy it, version it, do maintenance upgrades and bug fixes, etc.

About scheduling the jobs, then use cron to produce events in your system,
calling a service URL that triggers the processing of the next batch of
data:
  
~~~ cron
0 22 * * 1-5 curl https://service.internal.url/schedule/daily-reporting/
*/5 * * * *  curl https://matviews.internal.url/schedule/refresh-mat-views/
~~~

You can then browse to your interactive web application and see the
dashboard with the interesting metrics, a summary of what happened when, how
many users got a daily reporting, maybe even have a copy of the sent email,
etc.

## Conclusion

Some cron jobs are technical details meant for sysadmins, and then using
system logs to track the activity is fair enough. Some cron jobs are
implementing a user visible part of your business logic, and as such they
need to expose business metrics and allow for direct control of the running
tasks, their scheduling, and the processing itself.

The best way to achieve proper business logic background jobs scheduling is
to actually write a fully interactive server-side application (a web app,
typically), and have it expose an API call that inject a scheduler event in
the application.

When doing things that way, then cron is used for what it's good at:
producing an event at known regular timings, controled by the calendar.
