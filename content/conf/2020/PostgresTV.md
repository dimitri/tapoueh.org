+++
date = "2020-11-02T00:00:00.000000+02:00"
title = "Postgres Thursday s01e04: Dimitri Fontaine. The Art of PostgreSQL; pg_auto_failover"
categories = ["Conferences","PostgreSQL Confs"]
city = "Online"
slides = []
youtube = "Ztvst8IxtjE"
conference = "[Postgres.TV](https://www.youtube.com/channel/UCqX_FVBw4RL-DMDwZ_H86gg) is a series of online interviews organized by  Nikolay Samokhvalov and Ilya Kosmodemiansky"
summary = """In this interview we dive in my book [The Art of PostgreSQL](https://theartofpostgresql.com) and then in the High Availability and automated failover solution that I work on, [pg_auto_failover](https://github.com/citusdata/pg_auto_failover)."""
+++

This was one of the more enjoyable online conversations of the pandemic
year. Nikolay and Ilya are genuine PostgreSQL experts and the conversation
went deep quickly — less "tell us about your book" and more "why did you
choose to cover X before Y" and "what is the most misunderstood feature?"

The answer to that last question: window functions. Most readers assume
they are an advanced feature for data analysts only. In practice they solve
everyday application problems cleanly — ranking, running totals, lead/lag
comparisons — and every developer who writes non-trivial SQL should be
fluent in them.

The second half covers [pg_auto_failover](https://github.com/citusdata/pg_auto_failover), the HA extension that automates
PostgreSQL failover for application developers who want reliable production
Postgres without becoming HA experts themselves. The core insight: a
monitor process that holds the state machine for each node's role, so
failover is deterministic and observable, not a black-box race between
agents.
