+++
date = "2019-02-03T00:00:00.000000+02:00"
title = "FOSDEM 2019: Data Modeling, Normalization, and Denormalisation"
categories = ["Conferences","PostgreSQL Confs"]
city = "Bruxelles, Belgium"
slides = ["../../../images/confs/Fosdem_2019_data_modeling.png;../../../images/confs/Fosdem_2019_data_modeling.pdf"]
youtube = "059NNng5BUQ"
conference = "[FOSDEM](https://fosdem.org/2019/) is a free event for software developers to meet, share ideas and collaborate. Every year, thousands of developers of free and open source software from all over the world gather at the event in Brussels."
summary = """As a developer using PostgreSQL one of the most important tasks you have to deal with is modeling the database schema for your application. In order to achieve a solid design, it’s important to understand how the schema is then going to be used as well as the trade-offs it involves.

As Fred Brooks said: “Show me your flowcharts and conceal your tables, and I shall continue to be mystified. Show me your tables, and I won’t usually need your flowcharts; they’ll be obvious.”

In this talk we're going to see practical normalisation examples and their benefits, and also review some anti-patterns and their typical PostgreSQL solutions, including Denormalization techniques thanks to advanced Data Types."""
+++

FOSDEM is the largest gathering of open-source developers in Europe, and
the PostgreSQL devroom is always one of the most popular rooms at the
event — standing room only, with an audience that includes core hackers,
DBAs, and application developers in equal measure.

This talk grew out of conversations with readers of *The Art of
PostgreSQL* who consistently found data modeling to be the hardest chapter
to apply. The book covers normalisation as theory; the talk focuses on the
practice: what does a first normal form violation actually look like in a
production schema? What is the cost of an EAV table beyond the obvious
query complexity? What does PostgreSQL give you — `jsonb`, array types,
range types — when you do need to denormalise, and when should you reach
for those instead of fighting the relational model?

The Fred Brooks quote — "Show me your tables and I won't usually need your
flowcharts" — always lands well with this audience. Database schemas
encode business rules that are often invisible in the application layer;
a well-normalised schema makes those rules explicit and enforces them at
the lowest possible layer.

The talk was recorded and is available above. The slides (PDF) link to the
PostgreSQL documentation for every feature mentioned.
