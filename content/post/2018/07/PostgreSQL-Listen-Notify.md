+++
title = "PostgreSQL LISTEN/NOTIFY"
date = "2018-07-19T12:58:21+02:00"
tags = ["PostgreSQL","YeSQL","Concurrency","LISTEN","NOTIFY","GoLang","JSON"]
categories = ["PostgreSQL","YeSQL"]
coverImage = "/img/elephant.png"
coverMeta = "in"
coverSize = "partial"
thumbnailImage = "/img/siren_alarm_bell_signal_alert-512.png"
thumbnailImagePosition = "left"

+++

This article fits in the [PostgreSQL Concurrency](/tags/concurrency) series,
where we installed a tweeter like application schema and had all the
characters from Shakespeare's *A Midsummer Night's Dream* tweet their own
lines in our database in [PostgreSQL Concurrency: Data Modification
Language](/blog/2018/06/PostgreSQL-DML.md).

A previous article in the series covered how to manage concurrent retweets
in an efficient way: [Computing and
Caching](/blog/2018/07/computing-and-caching/), where we learn how to
maintain a cache right in your PostgreSQL database, thanks for materialized
views.

Today's article shows how to maintain an _external_ cache in another
application layer. In this article we are going to maintain an in-memory
cache in a Golang service, using PostgreSQL
[LISTEN](https://www.postgresql.org/docs/current/static/sql-listen.html) and
[NOTIFY](https://www.postgresql.org/docs/current/static/sql-notify.html)
features.

<!--more-->
<!--toc-->

## Listen and Notify

The PostgreSQL protocol includes a streaming protocol with *COPY* and also
implements asynchronous messages and notifications. This means that as soon
as a connection is established with PostgreSQL, the server can send messages
to the client even when the client is idle.

## PostgreSQL Notifications

Messages that flow from the server to the connected client should be
processed by the client. It could be that the server is being restarted, or
an application message is being delivered.

Here's an example of doing this:

~~~ sql
yesql# listen channel;
LISTEN

yesql# notify channel, 'foo';
NOTIFY
Asynchronous notification "channel" with payload "foo"  ⏎
received from server process with PID 40430.
~~~

Note that the message could be sent from another connection, so try it and
see with several *psql* instances. The *payload* from the message can be any
text, up to 8kB in length. This allows for rich messages to flow, such as
JSON encoded values.

## PostgreSQL Event Publication System

In the [Triggers](#triggers) section we saw that in order to maintain a
cache of the action counters either by day or by messageid, we can write a
trigger. This implements event driven processing but kills our concurrency
and scalability properties.

It's possible for our trigger to *notify* an external client. This client
must be a daemon program, which uses *listen* to register our messages. Each
time a notification is sent, the daemon program processes it as necessary,
possibly updating our *twcache.counters* table. As we have a single daemon
program listening to notifications and updating the cache, we now bypass the
concurrency issues.

Before implementing the client application, we can implement the trigger for
notification, and use *psql* as a testing client:

~~~ sql
begin;

create or replace function twcache.tg_notify_counters ()
 returns trigger
 language plpgsql
as $$
declare
  channel text := TG_ARGV[0];
begin
  PERFORM (
     with payload(messageid, rts, favs) as
     (
       select NEW.messageid,
              coalesce(
                 case NEW.action
                   when 'rt'    then  1
                   when 'de-rt' then -1
                  end,
                 0
              ) as rts,
              coalesce(
                case NEW.action
                  when 'fav'    then  1
                  when 'de-fav' then -1
                 end,
                0
              ) as favs
     )
     select pg_notify(channel, row_to_json(payload)::text)
       from payload
  );
  RETURN NULL;
end;
$$;

CREATE TRIGGER notify_counters
         AFTER INSERT
            ON tweet.activity
      FOR EACH ROW
       EXECUTE PROCEDURE twcache.tg_notify_counters('tweet.activity');

commit;
~~~

Then to test the trigger, we can issue the following statements at a *psql*
prompt:

~~~ psql
listen "tweet.activity";

insert into tweet.activity(messageid, action)
     values (33, 'rt'),
            (33, 'rt'),
            (33, 'de-rt'),
            (33, 'fav'),
            (33, 'de-fav'),
            (33, 'rt'),
            (33, 'fav');
~~~


We get then the following output from the console:

~~~ psql
INSERT 0 7
Asynchronous notification "tweet.activity" with payload        ⏎
"{"messageid":33,"rts":1,"favs":0}" received from              ⏎
server process with PID 73216.
Asynchronous notification "tweet.activity" with payload        ⏎
"{"messageid":33,"rts":-1,"favs":0}" received from             ⏎
server process with PID 73216.
Asynchronous notification "tweet.activity" with payload        ⏎
"{"messageid":33,"rts":0,"favs":1}" received from              ⏎
server process with PID 73216.
Asynchronous notification "tweet.activity" with payload        ⏎
"{"messageid":33,"rts":0,"favs":-1}" received from             ⏎
server process with PID 73216.
~~~

So we made seven inserts, and we have four notifications. This behavior
might be surprising, yet it is fully documented on the PostgreSQL manual
page for the
[NOTIFY](https://www.postgresql.org/docs/current/static/sql-notify.html)
command:

> If the same channel name is signaled multiple times from the same
> transaction with identical payload strings, the database server can decide
> to deliver a single notification only. On the other hand, notifications
> with distinct payload strings will always be delivered as distinct
> notifications. Similarly, notifications from different transactions will
> never get folded into one notification. Except for dropping later
> instances of duplicate notifications, NOTIFY guarantees that notifications
> from the same transaction get delivered in the order they were sent. It is
> also guaranteed that messages from different transactions are delivered in
> the order in which the transactions committed.

Our test case isn't very good, so let's write another one, and keep in mind
that our implementation of the cache server with *notify* can only be
correct if the main application issues only distinct *tweet.activity*
actions in a single transaction. For our usage, this is not a deal-breaker,
so we can fix our tests.

~~~ sql
insert into tweet.activity(messageid, action) values (33, 'rt');
insert into tweet.activity(messageid, action) values (33, 'de-rt');
insert into tweet.activity(messageid, action) values (33, 'fav');
insert into tweet.activity(messageid, action) values (33, 'de-rt');
~~~

And this time we get the expected notifications:

~~~ psql
Asynchronous notification "tweet.activity" with payload        ⏎
"{"messageid":33,"rts":1,"favs":0}" received from              ⏎
server process with PID 73216.
Asynchronous notification "tweet.activity" with payload        ⏎
"{"messageid":33,"rts":-1,"favs":0}" received from             ⏎
server process with PID 73216.
Asynchronous notification "tweet.activity" with payload        ⏎
"{"messageid":33,"rts":0,"favs":1}" received from              ⏎
server process with PID 73216.
Asynchronous notification "tweet.activity" with payload        ⏎
"{"messageid":33,"rts":-1,"favs":0}" received from             ⏎
server process with PID 73216.
~~~

<hr />

{{< figure class="right"
             src="/img/TAOP_Book_Cover_200x260.png"
            link="https://theartofpostgresql.com" >}}
            
This article is extracted from my book [The Art of
PostgreSQL](https://theartofpostgresql.com), which teaches SQL to developers
so that they may replace thousands of lines of code with very simple
queries. The book has a full chapter about *Data Manipulation and
Concurrency Control* in PostgreSQL, including caching with materialized
views, check it out!

<hr />

## Notifications and Cache Maintenance

Now that we have the basic server-side infrastructure in place, where
PostgreSQL is the server and a backend application the client, we can look
into about maintaining our *twcache.counters* cache in an event driven
fashion.

PostgreSQL LISTEN and NOTIFY support is perfect for maintaining a cache.
Because notifications are only delivered to client connections that are
listening at the moment of the notify call, our cache maintenance service
must implement the following behavior, in this exact order:

  1. Connect to the PostgreSQL database we expect notifications from and
     issue the *listen* command.
     
  2. Fetch the current values from their *single source of truth* and reset
     the cache with those computed values.
     
  3. Process notifications as they come and update the in-memory cache, and
     once in a while synchronize the in-memory cache to its materialized
     location, as per the cache invalidation policy.

The cache service can be implemented within the cache maintenance service.
As an example, a cache server application might both process notifications
and serve the current cache from memory over an HTTP API. The cache service
might also be one of the popular cache solutions such as
[Memcached](https://memcached.org) or [Redis](https://redis.io).

In our example, we implement a cache maintenance service in Go and the cache
itself is maintained as a PostgreSQL table:

~~~ sql
begin;

create schema if not exists twcache;

create table twcache.counters
 (
   messageid   bigint not null primary key,
   rts         bigint,
   favs        bigint
 );

commit;
~~~

With this table, implementing a NOTIFY client service that maintains the
cache is easy enough to do, and here's what happens when the service runs
and we do some testing:

~~~
2017/09/21 22:00:36 Connecting to postgres:///yesql?sslmode=disable… 
2017/09/21 22:00:36 Listening to notifications on channel "tweet.activity"
2017/09/21 22:00:37 Cache initialized with 6 entries.
2017/09/21 22:00:37 Start processing notifications, waiting for events…
2017/09/21 22:00:42 Received event: {"messageid":33,"rts":1,"favs":0}
2017/09/21 22:00:42 Received event: {"messageid":33,"rts":-1,"favs":0}
2017/09/21 22:00:42 Received event: {"messageid":33,"rts":0,"favs":1}
2017/09/21 22:00:42 Received event: {"messageid":33,"rts":-1,"favs":0}
2017/09/21 22:00:47 Materializing 6 events from memory
~~~

The client code is written in Go, and is included in full in this article.
One of the main bits of interest is the *materialize* function, which is an
interesting implementation of pushing the in-memory cache data structure
down to our PostgreSQL table *twcache.counters*.

The in-memory cache structure looks like the following:

~~~ go
type Counter struct {
	MessageId int `json:"messageid"`
	Rts       int `json:"rts"`
	Favs      int `json:"favs"`
}

type Cache map[int]*Counter
~~~

And given such a data structure, we use the efficient Go default JSON
marshaling facility to transform the cache elements and pass them all down
to PostgreSQL as a single JSON object.

~~~ go
func materialize(db *sql.DB, cache Cache) error {
    ...
    
	js, err := json.Marshal(cache)

	if err != nil {
		log.Printf("Error while materializing cache: %s", err)
		return err
	}
    
	_, err = db.Query(q, js)
    
    ...
    return nil
}
~~~

The JSON object is then processed in a SQL query, that we find embedded in
the Go code — it's the *q* string variable that is used in the snippet above
in the expression *db.Query(q, js)*, where *js* is the JSON representation
of the entirety of the cache data.

Here's the SQL query we use:

~~~ sql
with rec as
 (
   select rec.*
     from json_each($1) as t,
          json_populate_record(null::twcache.counters, value) as rec
 )
 insert into twcache.counters(messageid, rts, favs)
      select messageid, rts, favs
        from rec
 on conflict (messageid)
   do update
         set rts  = counters.rts + excluded.rts,
             favs = counters.favs + excluded.favs
       where counters.messageid = excluded.messageid
~~~

In this query, we use the PostgreSQL
[json_populate_record](https://www.postgresql.org/docs/9.6/static/functions-json.html#FUNCTIONS-JSON-PROCESSING-TABLE)
function. This function is almost magical and it is described as such in the
documentation:

> Expands the object in from_json to a row whose columns match the record
> type defined by base (see note below).

> Note: In *json_populate_record*, *json_populate_recordset*,
> *json_to_record* and *json_to_recordset*, type coercion from the JSON is
> "best effort" and may not result in desired values for some types. JSON
> keys are matched to identical column names in the target row type. JSON
> fields that do not appear in the target row type will be omitted from the
> output, and target columns that do not match any JSON field will simply be
> NULL.

The function allows transforming a JSON document into a full-blown
relational tuple to process as usual in PostgreSQL. Here we use an implicit
*lateral* construct that feeds the *json_populate_record()* function from
the output of the *json_each()* function. We could have used the *recordset*
variant, but we're discarding the Go cache key that repeats the *MessageId*
here.

Then our SQL query uses the *insert into ... select ... on conflict do
update* variant that we're used to by now.

So, here's the full Go script we're using here:

~~~ go
package main

import (
	"database/sql"
	"encoding/json"
	"flag"
	"time"
	"log"
	"os"

	"github.com/lib/pq"
)

type Config struct {
	PGuri   string
	Channel string
	Sync    time.Duration
	Idle    time.Duration
}

type Counter struct {
	MessageId int `json:"messageid"`
	Rts       int `json:"rts"`
	Favs      int `json:"favs"`
}

type Cache map[int]*Counter

const sync_delay = 10 * time.Second
const idle_delay = 30 * time.Second
const scrn_width = 65
const min_reconn = 10 * time.Second
const max_reconn = time.Minute

func main() {
	conf := processFlags()
	log.SetOutput(os.Stdout)

	log.Printf("Connecting to %s… ", conf.PGuri)
	db, err := sql.Open("postgres", conf.PGuri)
	if err != nil {
		log.Printf("Failed to connect to '%s': %s", conf.PGuri, err)
		os.Exit(1)
	}
	defer db.Close()

	reportErr := func(ev pq.ListenerEventType, err error) {
		if err != nil {
			log.Printf("Failed to start listener: %s", err)
			os.Exit(1)
		}
	}

	//
	// First, LISTEN to incoming events
	//
	listener := pq.NewListener(conf.PGuri, min_reconn, max_reconn, reportErr)
	err = listener.Listen(conf.Channel)
	if err != nil {
		log.Printf(
			"Failed to LISTEN to channel '%s': %s",
			conf.Channel, err)
		panic(err)
	}
	log.Printf(
		"Listening to notifications on channel \"%s\"",
		conf.Channel)

	//
	// Second, initialize the cache with the current values from the
	// base table, only then proceed to process the notifications.
	//
	cache, err := initCache(db)

	if err != nil {
		log.Printf("Error initializing cache")
		panic(err)
	}
	log.Printf(
		"Cache initialized with %d entries.",
		len(cache))

	//
	// Third, grab notifications and process them by updating the
	// counters that changed.
	//
	log.Println("Start processing notifications, waiting for events…")

	reset := time.Now()
	for {
		waitForNotification(listener, cache, conf.Idle)

		if time.Since(reset) >= conf.Sync {
			err := materialize(db, cache)

			// reset the cache *unless* there was an error!
			if err == nil {
				cache = make(Cache)
				reset = time.Now()
			}
		}
	}
}

func processFlags() *Config {
	var conninfo string = "postgres:///yesql?sslmode=disable"
	var channel string = "tweet.activity"

	syncPtr := flag.Int("sync", int(sync_delay.Seconds()),
		"Sync cache every SYNC seconds")
	flag.StringVar(&conninfo, "pguri", conninfo,
		"PostgreSQL connection string")
	flag.StringVar(&channel, "channel", channel,
		"LISTEN to this channel")

	flag.Parse()

	sync := time.Duration(*syncPtr) * time.Second
	idle := sync / 4

	return &Config{conninfo, channel, sync, idle}
}

func waitForNotification(l *pq.Listener, cache Cache, timeout time.Duration) {
	select {
	case n := <-l.Notify:
		var c Counter
		err := json.Unmarshal([]byte(n.Extra), &c)

		if err != nil {
			log.Printf("Failed to parse '%s': %s", n.Extra, err)
		} else {
			log.Printf("Received event: %s", n.Extra)
			updateCounter(cache, c)
		}

	case <-time.After(timeout):
		return
	}
}

func initCache(db *sql.DB) (Cache, error) {
	q := "select messageid, rts, favs from tweet.message_with_counters;"

	rows, err := db.Query(q)
	if err != nil {
		return nil, err
	}
	cache := make(Cache)

	for rows.Next() {
		c := Counter{}
		err := rows.Scan(&c.MessageId, &c.Rts, &c.Favs)

		if err != nil {
			return nil, err
		}
		cache[c.MessageId] = &c
	}
	err = rows.Err()
	if err != nil {
		return nil, err
	}
	return cache, nil
}

func materialize(db *sql.DB, cache Cache) error {
	if len(cache) == 0 {
		return nil
	}
	log.Printf("Materializing %d events from memory", len(cache))

	q := `
with rec as
 (
   select rec.*
     from json_each($1) as t,
          json_populate_record(null::twcache.counters, value) as rec
 )
 insert into twcache.counters(messageid, rts, favs)
      select messageid, rts, favs
        from rec
 on conflict (messageid)
   do update
         set rts  = counters.rts + excluded.rts,
             favs = counters.favs + excluded.favs
       where counters.messageid = excluded.messageid
`
	js, err := json.Marshal(cache)
	if err != nil {
		log.Printf("Error while materializing cache: %s", err)
		return err
	}

	_, err = db.Query(q, js)
	if err != nil {
		log.Printf("Error materliazing cache: %s", err)
		return err
	}
	return nil
}

func updateCounter(cache Cache, c Counter) {
	_, found := cache[c.MessageId]

	if found {
		cache[c.MessageId].Rts += c.Rts
		cache[c.MessageId].Favs += c.Favs
	} else {
		cache[c.MessageId] = &c
	}
}
~~~

It's important to note that coded as such, we can use the function to both
materialize a full cache as fetched at startup, and to materialize the cache
we build in-memory while receiving notifications.

The query used to fetch the initial value of the cache and set it again at
startup is the following:

~~~ sql
select messageid, rts, favs
  from tweet.message_with_counters;
~~~

We use the view definition that we saw earlier to do the computations for
us, and fill in our in-memory cache data structure from the result of the
query.

The trigger processing has a cost of course, as we can see in the following
test:

~~~ lisp
CL-USER> (concurrency::concurrency-test 100 100 35)
Starting benchmark for updates
Updating took 8.428939 seconds, did 10000 rts

Starting benchmark for inserts
Inserting took 10.351908 seconds, did 10000 rts
~~~

Remember when reading those numbers that we can't compare them meaningfully
anymore. We installed our trigger after insert on *tweet.activity*, which
means that the update benchmark isn't calling any trigger whereas the insert
benchmark is calling our trigger function 10,000 times in this test.

About the concurrency, notifications are serialized at commit time in the
same way that the PostgreSQL commit log is serialized, so there's no extra
work for PostgreSQL here.

Our cache maintenance server received 10,000 notifications with a JSON
payload and then reported the cumulated figures to our cache table only
once, as we can see from the logs:

~~~
2017/09/21 22:24:06 Received event: {"messageid":35,"rts":1,"favs":0}
2017/09/21 22:24:06 Received event: {"messageid":35,"rts":1,"favs":0}
2017/09/21 22:24:06 Received event: {"messageid":35,"rts":1,"favs":0}
2017/09/21 22:24:06 Received event: {"messageid":35,"rts":1,"favs":0}
2017/09/21 22:24:09 Materializing 1 events from memory
~~~

Having a look at the cache, here's what we have:

~~~ sql
table twcache.counters;
~~~

~~~ psql
 messageid │  rts   │ favs 
═══════════╪════════╪══════
         1 │  41688 │    0
         2 │ 222690 │    0
         3 │  22000 │    0
        33 │     -4 │    8
         5 │   7000 │    0
         6 │  30000 │    0
        35 │  10000 │    0
(7 rows)
~~~

We can see the results of our tests, and in particular, the message with
*ids* from 1 to 6 are in the cache as expected. Remember the rules we
introduced earlier where the first thing we do when starting our cache
maintenance service is to *reset* the cache from the real values in the
database. That's how we got those values in the cache; alter all, the cache
service wasn't written when we ran our previous series of tests.

## Limitations of Listen and Notify

It is crucial that an application using the PostgreSQL notification
capabilities are capable of missing events. Notifications are only sent to
connected client connections.

Any queueing mechanism requires that event accumulated when there's no
worker connected are kept available until next connection, and replication
is a special case of event queueing. It is not possible to implement
queueing correctly with PostgreSQL *listen/notify* feature.

A cache maintenance service really is the perfect use case for this
functionality, because it's easy to reset the cache at service start or
restart.

## Listen and Notify Support in Drivers

Support for listen and notify PostgreSQL functionality depends on the driver
you're using. For instance, the Java JDBC driver documents the support at
[PostgreSQL™ Extensions to the JDBC
API](https://jdbc.postgresql.org/documentation/head/listennotify.html), and
quoting their page:

> A key limitation of the JDBC driver is that it cannot receive
> asynchronous notifications and must poll the backend to check if any
> notifications were issued. A timeout can be given to the poll function,
> but then the execution of statements from other threads will block.

There's still a full-length class implementation sample, so if you're using
Java check it out.

For Python, the [Psycopg](http://initd.org/psycopg/) driver is the most
popular, and [Python asynchronous
notifications](http://initd.org/psycopg/docs/advanced.html#asynchronous-notifications)
supports advanced techniques for avoiding *busy looping*:

> A simple application could poll the connection from time to time to check
> if something new has arrived. A better strategy is to use some I/O
> completion function such as select() to sleep until awakened by the kernel
> when there is some data to read on the connection, thereby using no CPU
> unless there is something to read.

The Golang driver [pq](https://godoc.org/github.com/lib/pq) also supports
[notifications](https://godoc.org/github.com/lib/pq#hdr-Notifications) and
doesn't require polling. That's the one we've been using this driver in our
example here.

For other languages, please check the documentation of your driver of choice.

## Conclusion

PostgreSQL implements several facilities that we can rely on to maintain an
application cache for data that changes often:

  - In [Computing and Caching](/blog/2018/07/computing-and-caching/), we saw
    how to use a MATERIALIZED VIEW to get a fixed snapshot of the data, and
    then easily REFRESH it to implement our **cache invalidation policy**.
    
    This technique is well adapted to use cases where you want to rebuild
    your cache every once in a while, maybe every night, or several times a
    day, down to maybe every five minutes if the refreshing of the cache is
    really fast.
    
  - In [PostgreSQL Event Based
    Processing](/blog/2018/07/postgresql-event-based-processing/) we saw how
    to use TRIGGERs to maintain a transactionally correct cache, and the
    impact of such a choice on the scalability properties of your database
    backend.
    
    This solution is well suited to use case where the application only
    receives a small amount of UPDATE traffic, and quite far apart, and
    can't tolerate any lag when using the cache.
    
  - In today's article [PostgreSQL
    LISTEN/NOTIFY](/blog/2018/07/postgresql-listen/notify/) we saw how to
    build an online cache maintenance service with PostgreSQL's advanced
    notification features.
    
    This solution is well suited to use cases where a small amount of lag
    can be tolerated, up to maybe some seconds, most typically measured in
    the hundreds of milliseconds.

Again, we see that core PostgreSQL features allow application developers to
build exactly the facility they need. PostgreSQL really is
[YeSQL](/tags/yesql/)!
