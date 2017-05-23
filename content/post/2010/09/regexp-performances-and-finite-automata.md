+++
date = "2010-09-26T21:00:00.000000+02:00"
title = "Regexp performances and Finite Automata"
tags = ["PostgreSQL", "Emacs"]
categories = ["Emacs","Emacs Tips"]
thumbnailImage = "/img/postgresql-512.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/postgresql-512.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2010/09/26-regexp-performances-and-finite-automata",
           "/blog/2010/09/26-regexp-performances-and-finite-automata.html"]
+++

The major reason why I dislike 
[perl](http://www.perl.org/) so much, and 
[ruby](http://www.ruby-lang.org) too, and the thing I'd
want different in the 
[Emacs Lisp](http://www.gnu.org/software/emacs/manual/elisp.html) 
`API` so far is how they set developers mind
into using 
[regexp](http://www.regular-expressions.info/). You know the quote, don't you?

> Some people, when confronted with a problem, think “I know, I'll use regular
> expressions.” Now they have two problems.


That said, some situations require the use of 
*regexp* — or are so much
simpler to solve using them than the maintenance hell you're building here
ain't that big a drag. The given expressiveness is hard to match with any
other solution, to the point I sometime use them in my code (well I use 
[rx](http://www.emacswiki.org/emacs/rx)
to lower the burden sometime, just see this example).

~~~
(rx bol (zero-or-more blank) (one-or-more digit) ":")
"^[[:blank:]]*[[:digit:]]+:"
~~~


The thing you might want to know about 
*regexp* is that computing them is an
heavy task usually involving 
*parsing* their representation, 
*compiling* it to
some executable code, and then 
*executing* generated code. It's been showed in
the past (as soon as 1968) that a 
*regexp* is just another way to write a
finite automata, at least as soon as you don't need 
*backtracking*. The
writing of this article is my reaction to reading
[Regular Expression Matching Can Be Simple And Fast](http://swtch.com/~rsc/regexp/regexp1.html) (but is slow in Java,
Perl, PHP, Python, Ruby, ...), a very interesting article — see the
benchmarks in there.

The bulk of it is that we find mainly two categories of 
*regexp* engine in the
wild, those that are using 
[NFA](http://en.wikipedia.org/wiki/Nondeterministic_finite_state_machine) and 
[DFA](http://en.wikipedia.org/wiki/Deterministic_finite_automaton) intermediate representation
techniques, and the others. Our beloved 
[PostgreSQL](http://www.postgresql.org/) sure offers the feature,
it's the 
`~` and 
`~*` 
[operators](http://www.postgresql.org/docs/9.0/interactive/functions-matching.html). The implementation here is based on
[Henry Spencer](http://www.arglist.com/regex/)'s work, which the aforementioned article says

> became very widely used, eventually serving as the basis for the slow
> regular expression implementations mentioned earlier: Perl, PCRE, Python,
> and so on.


Having a look at the actual implementation shows that indeed, current
PostgreSQL code for 
*regexp* matching uses intermediate representations of
them as 
`NFA` and 
`DFA`. The code is quite complex, even more than I though it
would be, and I didn't have the time it would take to check it against the
proposed one from the 
*simple and fast* article.

~~~
postgresql/src/backend/regex
  -rw-r--r--   1 dim  staff   4362 Sep 25 20:59 COPYRIGHT
  -rw-r--r--   1 dim  staff    614 Sep 25 20:59 Makefile
  -rw-r--r--   1 dim  staff  28217 Sep 25 20:59 re_syntax.n
  -rw-r--r--   1 dim  staff  16589 Sep 25 20:59 regc_color.c
  -rw-r--r--   1 dim  staff   3464 Sep 25 20:59 regc_cvec.c
  -rw-r--r--   1 dim  staff  25036 Sep 25 20:59 regc_lex.c
  -rw-r--r--   1 dim  staff  16845 Sep 25 20:59 regc_locale.c
  -rw-r--r--   1 dim  staff  35917 Sep 25 20:59 regc_nfa.c
  -rw-r--r--   1 dim  staff  50714 Sep 25 20:59 regcomp.c
  -rw-r--r--   1 dim  staff  17368 Sep 25 20:59 rege_dfa.c
  -rw-r--r--   1 dim  staff   3627 Sep 25 20:59 regerror.c
  -rw-r--r--   1 dim  staff  27664 Sep 25 20:59 regexec.c
  -rw-r--r--   1 dim  staff   2122 Sep 25 20:59 regfree.c
~~~


So all in all, I'll continue avoiding 
*regexp* as much as I currently do, and
will maintain my tendency to using 
[awk](http://www.gnu.org/manual/gawk/gawk.html) when I need them on files (it allows
to refine the searching without resorting to more and more pipes in the
command line). And as far as resorting to using 
*regexp* in PostgreSQL is
concerned, it seems that the code here is already about topnotch. Once more.
