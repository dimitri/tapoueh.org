+++
date = "2011-10-10T10:35:00.000000+02:00"
title = "Extensions, applications"
tags = ["PostgreSQLFr", "Extensions", "Conferences"]
categories = ["Conferences","PostgreSQLFr Confs"]
thumbnailImage = "/img/old/conferences.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/conferences.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/10/10-extensions-applicatives",
           "/blog/2011/10/10-extensions-applicatives.html"]
+++

La 
[conférence PostgreSQL annuelle en Europe](http://2011.pgconf.eu/) a lieu la semaine prochaine à
Amsterdam, et j'espère que vous avez déjà vos billets, car cette édition
s'annonce comme un très bon millésime !

Je présenterai donc comment utiliser les extensions, le titre en anglais est
[Extensions are good for business logic](http://www.postgresql.eu/events/schedule/pgconfeu2011/session/138-extensions-are-good-for-business-logic/), et l'idée est de voir comment
exploiter les extensions afin de mieux gérer vos mises à jours en bases de
données.

Le cycle de vie des bases de données en production inclue souvent
l'utilisation d'une base de développement où le schéma évolue au rythme des
besoins des développeurs, et de temps en temps on consolide une partie de
ces modifications (dans des 
*rollouts*, scripts contenant principalement des
`DDL`) afin de les déployer en production — si possible avec une étape
intermédiaire en préproduction, tout de même.

Savoir ce qui est déployé en développement et comment en retirer le script à
jouer en production peut être parfois fastidieu.  Quand ce n'est pas le cas,
c'est que le travail a été fait en amont, ce qui est le signe d'une bonne
organisation, avec les surcoûts que l'on peut imaginer.

Les 
[extensions](http://www.postgresql.org/docs/9.1/static/extend-extensions.html) telles que présentes dans PostgreSQL 9.1 vous permettent de
mieux gérer ce genre de cas, en optimisant le surcoût : il ne disparaît pas,
mais devient opérationnel plutôt que de rester une charge d'organisation.

Allez, je vous laisse maintenant, je dois me préparer pour la conférence :)
