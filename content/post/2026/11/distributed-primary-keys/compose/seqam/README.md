# Sequence access methods: history, and the tests behind the article

Details that the article "Distributed primary keys" summarises in one section: the
history of the two patch sets, the `snowflake` method that the second one
includes, and the full table of tests on it, on `regionseq` (`../../extension/`),
on the article's expression and on `uuidv7()`. Results are in `../../results/`;
the scripts that made them are in `..` (see its README).

Postgres has no hook for how a sequence computes its next value. Two patch
sets tried to add one, with the same aim: let a `serial` or identity column be
fed by something other than a counter, and so give distributed systems globally
unique ids without an expression in every column default.

The first one, SeqAM, began in January 2013 on pgsql-hackers, from Simon Riggs,
with a `CREATE SEQUENCE ... USING` clause, a `pg_seqam` catalog and a built-in
`local` method, for clustering.
[The thread](https://postgrespro.com/list/thread-id/1814639) shows where it
caught: Heikki Linnakangas objected in November 2013 that the API leaked the
on-disk format and the WAL records to the plugin, and wanted the backend to keep
caching and logging with the plugin only allocating values; Andres Freund
answered that BDR had built distributed sequences on exactly that API. Petr
Jelinek rewrote it around Heikki's allocation callback in 2014, Heikki asked for
a second, real method to test the API against, and [the CommitFest
entry](https://commitfest.postgresql.org/patch/44/) was returned with feedback
in February and again in July 2015. In August 2016 Petr wrote that he wouldn't
revive it [until there was built-in logical
replication](https://postgrespro.com/list/thread-id/1899415), and Peter
Eisentraut's `pg_sequence` catalog, which removed the properties from the
sequence's heap, was committed that December.

The second, ["Sequence Access Methods, round
two"](https://commitfest.postgresql.org/patch/4677/), is Michael Paquier's, from
December 2023, built on that catalog: a sequence AM may store nothing, or keep
its state in any table AM, with its own WAL if it wants; the built-in behaviour
becomes a `local` method; `default_sequence_access_method` picks the method for
`serial` and identity columns; and `pg_dump` reads sequences through
`pg_sequence_last_value()` instead of their heap. Some of its refactoring parts
were committed, in February 2024 and August 2025, and the rest went through 29
versions and twelve CommitFests. The reviews asked for the use case before the
code: Peter Eisentraut asked for more than two lines of use cases, and for a real
implementation instead of a dummy module, and Tomas Vondra doubted that
active-active systems want this at all, since they mostly pick UUIDs. The 2024
pgconf.dev unconference reached no consensus, and in a poll about half of the
answers found 64-bit values enough. The entry was closed as returned with
feedback on 2026-02-27, and on 2026-05-16, after a v29 and a benchmark from
Andrei Lepikhov, Paquier wrote that he had dropped the series from the
CommitFest: little enthusiasm, and "zero feedback" from other core developers.
What the threads show missing, in both rounds, is a use case with a committer
behind it. The multi-region key of this article is one, so we ran the v29
patches (`compose/seqam/Dockerfile` builds the Postgres master of 2026-05-14
with them, without assertions; its `snowflake` and core regression suites pass)
and then wrote a method of our own for it.

## The method in the patches: `snowflake`

The set includes a `contrib/snowflake` method, the demonstration of the API. It
makes a Snowflake id, the layout that Twitter published in 2010: 64 bits, the
sign unused, so 63 are left for 41 bits of milliseconds, 10 of a machine number
and 12 of a counter. The state of the sequence is one 16-bit counter in a
one-row unlogged relation, and `nextval` does this:

1. lock the page of that relation;
2. add 1 to the counter; if it passes 4,095, set it back to 1 and sleep for 1 ms;
3. read the clock in milliseconds;
4. put the milliseconds, the machine number from the superuser setting
   `snowflake.machine_id` (0 to 1,023, default 1) and the counter together;
5. write the counter back and unlock.

Two ids differ if they come from different machines, or from different
milliseconds, or have different counters, and the sleep is what keeps the third
true: after 4,095 ids at least a millisecond has passed, so the counter can't
come round to the same value within one. A machine can make 4,096 ids per
millisecond, 4.1 million a second. Nothing is written to the WAL.

Run on the lab, it did what it says, and showed what a demonstration leaves out
(`results/101-seqam-func.out`):

- *Columns narrower than 64 bits.* An `int` identity column gets a sequence
  typed `integer` with maximum 2,147,483,647, which the method doesn't look at:
  `nextval` returns the 64-bit id and the column keeps its low 32 bits, without
  an error. The first row held -1237315582, and a duplicate key stopped the
  inserts after 634,724 rows. A `smallint` column stopped after 4,094. A plain
  sequence says `nextval: reached maximum value` instead.
- *`MAXVALUE`* isn't enforced either. After `MAXVALUE 100` the sequence went on
  to hand out 64-bit ids, and a `pg_dump` taken once the counter was past 100
  doesn't restore: `setval: value 151 is out of bounds for sequence "sf"
  (1..100)`.
- *The machine number* is a setting of the session, with 1 as its default, and
  nothing checks that two servers differ (the mesh below).
- *The clock set back* isn't handled, and a clock that is a few milliseconds
  behind repeats ids (the table below).
- *Speed.* The page is locked exclusively for the whole call, the clock is read
  inside the lock, and the 1 ms sleep happens inside it too: a million `nextval`
  take about 620 ms in one session against 145 for a plain sequence, and at
  least 244 ms of that are the sleeps. 1.3 million values a second whether 1, 4
  or 16 clients ask (`results/105-seqam-scale.out`).

Dump and restore otherwise work, and a crash (`kill -9` of the checkpointer)
resets the counter to 1 with ids that are still higher, because the time is.

## A method of our own: `regionseq`

The `extension/` directory of the article is a sequence access method for the
multi-region key, written against the same API and shipped as an extension: a
`Makefile` that uses PGXS, a control file, a SQL script, 527 lines of C, a
regression test and a README. The Dockerfile builds it out of tree, the way a
user would, against the patched server that is installed. What it keeps from the
`snowflake` method is the layout idea, 41 bits of milliseconds then the region
then a counter; the rest answers what the run above found.

The id is 63 bits, from the top: 41 of milliseconds since 2026-01-01 (until
2095), 6 of region number, 16 of counter. The region comes from
`regionseq.region_id`, a setting that is fixed for the life of the server and
has no default: until it is set, `nextval` fails, and without
`shared_preload_libraries = 'regionseq'` the sequence can't be used at all. The
state is the last millisecond and counter handed out, and `nextval` does this,
with the page locked:

~~~ c
now = clock_ms()
if now > last_ms:   ms = now;      counter = 0           # the clock moved on
else:               ms = last_ms;  counter = counter + 1 # same millisecond, or the clock went back
                    if counter > 65535: ms = ms + 1; counter = 0   # take the next ms, don't wait for it
if last_ms - now > regionseq.max_clock_regression: error # 60 s by default
if (region << 16 | counter | ms << 22) > MAXVALUE:  error
if ms >= high_water or first id since the checkpoint:
    log to the WAL: "everything below high_water = ms + 1000 ms is used"
(last_ms, counter) = (ms, counter); return ms << 22 | region << 16 | counter
~~~

Four properties come from that:

- **Ids of a region increase whatever the clock does.** A clock that goes back
  doesn't repeat an id: the sequence keeps the last millisecond and counts on
  inside it, and the next millisecond is borrowed when the counter runs out, at
  65,536 ids a millisecond. Until the clock is past the last millisecond again,
  the ids run a little ahead of it. A clock more than a minute behind is an error
  rather than a long crawl.
- **No sleep, no wait.** The call holds the lock for the time of an addition, so
  it costs what a plain sequence costs.
- **The state survives a crash and a promotion** the way a plain sequence's does,
  with one WAL record, a full page image, a second at most instead of one per 32
  values: the WAL says that everything below the high-water mark is used, and
  after a crash or on a promoted standby the ids resume at that mark or at the
  clock, whichever is later.
- **A configuration mistake is an error.** `MAXVALUE` is honoured, so an `int`
  identity column fails at its first `nextval` with a hint to use `bigint`;
  `INCREMENT` and `CACHE` other than 1 are errors; the region number can't be
  forgotten, only chosen twice.

`setval()` reads the time and the counter of the id it is given, so `pg_dump`
and a restore into another region carry the position and not the region. Three
SQL functions decode an id, and a publication's row filter, which can't call
them, can use the operators they are made of.

## The same tests on the three, and on the expression

All on the patched build, in the lab of the article. One server's index
is that of one region: four writers, but one region number, which is what a
region's own writers are.

| | `snowflake` method | Expression (ours, the fifth key) | `regionseq` | `uuidv7()` |
|---|---|---|---|---|
| Rows/s, 4 writers, 5 M rows | 992,472 | 1,477,606 | 1,539,386 | 1,336,588 |
| Primary key index, one region | 112 MB | 112 MB | 112 MB | 158 MB |
| WAL per row | 165 B | 168 B | 165 B | 182 B |
| `nextval` alone, 1 and 16 clients | 1.3 M/s, 1.3 M/s | | 5.9 M/s, 4.5 M/s (plain sequence 6.5, 4.5) | |
| After a failover, six runs | first id above the last, by 290 to 350 ms | same, by 310 to 350 ms | above, by 975 ms (streaming) or 280 to 300 ms (cut off) | |
| Three regions, distinct numbers | converge, no conflict | converge | converge: 597,800 rows, same digest | converge |
| The same number everywhere | stops on `insert_exists` | | stops on `insert_exists` | |
| The number left at its default | runs, as region 1 | | `nextval` refuses | |
| Clock back 50 ms | 17 to 39 of about 19,300 transactions fail | 0 | 0 | 0 |
| Clock back 200 ms | 82 to 168 fail | 0 | 0 | 0 |
| Clock back 2 s | 1,587 to 1,973 fail (8.5% to 10.6%) | 183 to 197 of about 71,000 (0.26%) | 0 | 0 |
| Clock back 120 s | | | refuses until the clock catches up | |
| Crash, the clock set back 10 s before it | | | 200,000 ids, no duplicate, first one 778 ms above the last | |
| `int` column | keeps 32 bits of the id, duplicates after 634,724 rows | `integer out of range` | `reached maximum value`, with a hint | no such column |

Reading across, in the order of the rows:

- **Cost.** All three of the bigint keys build the same table and the same
  index: 112 MB when one region writes, against 148 to 150 MB when four regions
  write the same table, which is the cost of interleaving that the article's one-server
  section measured and has nothing to do with the method. `regionseq` is the
  fastest by a few percent, within the spread of the runs, and writes the least
  WAL with the `snowflake` method, whose counter isn't logged at all: its record
  is one a second, where a plain sequence logs once per 32 values. The
  `snowflake` method is a third slower, for the lock and the sleep.
- **Failover.** Time moves on, so none of the three repeats an id; the other
  plain sequences of the article's failover section repeat 300,000 numbers. `regionseq`
  skips ahead by up to a second after a promotion, which is the high-water mark
  of its WAL record: that's what it pays to stay safe when the clock of the new
  primary is behind.
- **The number of the region** is where the methods differ in kind. Written in a
  column default, as the expression has it, the number belongs to a table; in a
  setting that has a default, as `snowflake` has it, it belongs to a server that
  can silently share it, with the result of the mesh run: with 1 on all three
  servers, the regions stopped at 197,600, 99,500 and 192,400 rows, each with 32
  conflicts; with 1, 2 and 3, all three held the same 597,800 rows
  (`results/103-*`, `results/113-*`). In a setting without a default, the
  mistake is an error that the first insert shows. Two servers given the same
  number is still a mistake, which the subscriptions show as conflicts.
- **The clock** was stepped back while four clients inserted, three runs of each
  size, with libfaketime on the server (`results/104-*`; a failed transaction
  loses its 100 rows). The 12-bit counter of the `snowflake` method repeats an
  id once the clock is as little as 50 ms behind; the 18-bit counter of the
  expression, once it is seconds behind; `regionseq` and `uuidv7()` never did.
  For the first two, an id repeats when a millisecond comes round again with the
  same counter value, and a wider counter lines up less often. At 10,000 ids a
  second, one run of each with a 2 s step, none failed.
  (The rows/s of this test are slowed by libfaketime and don't compare with the
  table's first row. An earlier version of this table gave 50 and 200 ms for
  steps that libfaketime had read as minutes.)
- **The width of the column** is the difference that matters for upgrades: of the
  four, the one that keeps part of an id without telling is the one that has no
  `MAXVALUE` check.
