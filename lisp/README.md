% The Common Lisp backend for tapoueh.org
% Dimitri Fontaine <dimitri@2ndQuadrant.fr>
% version 1.0

# Tapoueh.org

The goal of this development is to allow maintaining a website including a
blog all from Emacs, with two modes of operation: all dynamic so that it's
easy to browse new material as it's added (new articles, tags, whatever);
and all static so that it's easy to run on production.

## Muse markup for articles

That's the best markup for editing articles from within Emacs.

## Common Lisp

That's the best language to develop software with, that I found.

## Server Side Include

That allows to have easy to maintain static pages with dynamic blocs.

## Publishing in style

We use Bootstrap / FontAwesome / JQCloud. The problem is that the lisp code
is well aware of that, and some bits of those styling parts are appearing
all over the code base: modularity violation, big time.

## TODO

  - sitemap.xml
  - /tags/index.html
  - update conferences page
  - blog about something
