+++
date = "2017-05-30T14:11:23" 
title = "The Sort Interview Question"
categories = ["PostgreSQL","Software Programming"]
tags = ["PostgreSQL","Software Programming","Order By","Interviews"]
coverImage = "/img/sort.png"
coverMeta = "out"
coverSize = "partial"
thumbnailImage = "/img/sort.png"
thumbnailImagePosition = "left"
draft = true
+++

According
to
[Geeks for Geeks](http://www.geeksforgeeks.org/top-10-algorithms-in-interview-questions/) number
4 of the top ten interview questions are all about *Sorting and Searching*.
This topic is rich with many aspects to consider: time and space complexity
of course, but also adaptation to the input size and *kind*.

I will argue here that the best answer to *sorting* a list of about anything
might well be an `ORDER BY` SQL statement.

<!--more-->
<!--toc-->

## Why sorting is an interesting topic

Sorting
is
[Chapter 5](https://en.wikipedia.org/wiki/The_Art_of_Computer_Programming#Volume_3.C2.A0.E2.80.93_Sorting_and_Searching) of
Knuth's
masterpiece
[The Art of Computer Programming](https://en.wikipedia.org/wiki/The_Art_of_Computer_Programming) and
just by having a look at the table of contents it's easy to understand that
a lot of thinking is required to address this problem.

When thinking about book shelves, kitchen cupboards, desks, parking slots or
streets, it seems like everything we deal with in our daily lifes need to be
sorted for us to be able to make sense of what's in there, or just to find
our car again.


## Sorting Corner Cases

Back to the 

## Sort by more than one criteria

## Data Set does not fit into memory

When the data set doesn't fit into memory then we need to
contemplate
[External Sorting](https://en.wikipedia.org/wiki/External_Sorting), which is
a complex matter. The main idea is that you're going to sort batches at a
time and store them out of memory (in *tapes*), then *merge* the tapes
together until you get the sorted result. Of course, tapes have no support
for random access and you need your algorithm to only ever read *forward*.

This is a solved problem thanks to 

old [tape sort](https://doxygen.postgresql.org/tuplesort_8c_source.html), or
in its full
name
[Polyphase Merge Sort](https://en.wikipedia.org/wiki/Polyphase_merge_sort):

> See Knuth, volume 3, for more than you want to know about the external
> sorting algorithm.  Historically, we divided the input into sorted runs
> using replacement selection, in the form of a priority tree implemented
> as a heap (essentially his Algorithm 5.2.3H), but now we only do that
> for the first run, and only if the run would otherwise end up being very
> short.  We merge the runs using polyphase merge, Knuth's Algorithm
> 5.4.2D.  The logical "tapes" used by Algorithm D are implemented by
> logtape.c, which avoids space wastage by recycling disk space as soon
> as each block is read from its "tape".


## Number, Bytes, and Unicode Strings

## Top-N Sorting, and KNN

## Conclusion

If you really want to solve a practical use case for sorting data, then
the [PostgreSQL](https://www.postgresql.org) `ORDER BY` implementation has
you covered in most cases. If you want to shine in your next interview when
asked about sorting, then you should know the complexities of different
algorithms for sure
