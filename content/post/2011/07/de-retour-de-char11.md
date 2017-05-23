+++
date = "2011-07-13T17:30:00.000000+02:00"
title = "De retour de CHAR(11)"
tags = ["PostgreSQLFr", "Conferences", "Skytools"]
categories = ["Conferences","PostgreSQLFr Confs"]
thumbnailImage = "/img/old/conferences.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/conferences.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/07/13-de-retour-de-char11",
           "/blog/2011/07/13-de-retour-de-char11.html"]
+++

Quelle meilleure occupation dans le train du retour de 
[CHAR(11)](http://char11.org/schedule) que de se
faire reporteur pour l'occasion ?  En réalité, dormir serait une idée tant
les soirées se sont prolongées !

Nous avons eu le plaisir d'écouter 
***Jan Wieck*** présenter un historique
simplifié de la réplication avec 
[PostgreSQL](http://www.postgresql.org/).  Étant lui-même l'un des
pionniers du domaine, son point de vue est des plus intéressants.  Il a
parlé de l'évolution des solutions de réplication, et je ne peux m'empêcher
de penser que par bien des côtés 
[Skytools](http://wiki.postgresql.org/wiki/SKytools) est une évolution de 
[Slony](http://slony.info/) — Jan,
auteur de Slony, semblait d'accord avec cela.

En effet Skytools est né de limitations de Slony.  Certaines d'entre elles
existent toujours, comme l'absence de séparation entre la couche de 
***queuing***
et la couche de réplication elle-même, et certaines ont été résolues depuis,
comme les difficultés à subir de fortes charges en écriture.  Et puis les
deux solutions partagent même une partie de leur implémentation, depuis
PostgreSQL 8.3, avec les types de données 
`txid` et 
[txid_snapshot](http://www.postgresql.org/docs/8.3/interactive/functions-info.html#FUNCTIONS-TXID-SNAPSHOT).  Bien sûr,
l'objectif de Skytools est d'avoir une solution la plus simple possible,
parfaitement adapée à un ensemble de cas d'utilisation précis et bornés,
alors que Slony essaye de résoudre automatiquement les problèmes les plus
difficiles du domaine, au prix d'une interface très complexe.

Bien sûr, 
***Jan*** a pris le temps de comparer objectivement ces solutions de
réplication avec la solution intégrée dans PostgreSQL, 
*Streaming Replication*
et 
*Hot Standby*.  Nous avions déjà la réplication binaire asynchrone,
PostgreSQL 9.1 nous apporte la réplication synchrone avec un contrôle par
transaction.  
[Simon Riggs](http://database-explorer.blogspot.com/), auteur de la fonctionalité, a insisté sur
l'innovation que cela représente : aucun autre projet ne permet de contrôler
la garantie de durabilité des données avec une granularité aussi souple et
précise !

[repmgr](http://projects.2ndquadrant.com/repmgr) est une solution d'administration de 
*cluster* animés avec 
*Hot Standby*
et 
*Streaming Replication* (synchrone ou non).  Son fonctionnement a été
détaillé par 
***Greg Smith*** et 
***Cédric Villemain***.  Le premier a montré comment
mettre au point une architecture permettant de répartir la charge en
lecture, et le second comment obtenir un système tolérant aux pannes grâce
au 
*failover* automatique intégré dans repmgr. Cette solution innovante a été
mise au point en grande partie par 2ndQuadrant France, nous l'avons déjà
estampillée 
*production ready*.

***[Magnus Hagander](http://www.hagander.net/)*** a beaucoup travaillé sur le protocole de 
*streaming* utilisé
pour la réplication intégrée dans PostgreSQL 9.1, ainsi que sur les outils
qui exploitent ce protocole.  Il a naturellement présenté cela, et l'idée
d'un 
*proxy* relayant le flux binaire des journaux de transaction est revenue
dans les discutions (nous avions déjà envisagé cela en 2010, l'article en
anglais 
[Back from PgCon2010](../../2010/05/27-back-from-pgcon2010.html) contient quelques éléments sur le sujet).  Avec
la réplication synchrone, il devient possible de concevoir des architectures
avancées, robustes et versatiles — le proxy pourrait maintenant s'occuper à
la fois des archives et des serveurs 
*standby*.

[Simon Riggs](http://database-explorer.blogspot.com/) nous a ensuite proposé une rétrospective des 7 dernières années
de travail qu'il a réalisé avec PostgreSQL, de l'implémentation du 
*Point in
Time Recovery* à la réplication synchrone, en passant par 
*Hot Standby*.  Ce
que nous avons dans PostgreSQL 9.0 correspond déjà à ce qu'Oracle propose de
plus avancé en terme de durabitilé des données, et 9.1 permet de franchir
l'étape suivante.  Cela ne freine en rien 
***Simon*** qui parlait déjà des projets
à venir pour les 10 prochaines années.

Enfin, 
[Heroku](http://www.heroku.com/) nous a présenté leur incroyable entreprise.  Ils ont
aujourd'hui plus de 
`150 000` instances de PostgreSQL en production,
démontrant que notre 
`SGBD` préféré est prêt pour les hébergeurs. 
***Heroku*** est
en train de concevoir et réaliser une solution prête à l'emploi pour le
fameux 
*Cloud* si difficile à définir.  Ici, il s'agit d'être capable
d'ajouter des nouveaux réplicas en lecture seule à la volée pour encaisser
les pics de trafic, créer des instances de développement d'un clic, etc.

Cet article ne couvre qu'une petite sélection des sujets abordés à la
conférence, je compte sur 
[Guillaume](http://blog.guillaume.lelarge.info/) pour lui aussi vous parler de 
[CHAR(11)](http://char11.org/schedule),
mais il faudra peut être attendre son retour des 
[RMLL](http://2011.rmll.info/) (quelle énergie !).
