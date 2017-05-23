+++
date = "2011-07-04T20:15:00.000000+02:00"
title = "Prêt pour CHAR(11) ?"
tags = ["PostgreSQLfr", "Conferences", "skytools"]
categories = ["Conferences","PostgreSQLFr Confs"]
thumbnailImage = "/img/old/conferences.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/conferences.jpg"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/07/04-pret-pour-char11",
           "/blog/2011/07/04-pret-pour-char11.html"]
+++

La semaine prochaine 
**déjà** se tient 
[CHAR(11)](http://www.char11.org/), la conférence spécialisée sur
le 
*Clustering*, la 
*Haute Disponibilité* et la 
*Réplication* avec 
[PostgreSQL](http://www.postgresql.org/).
C'est en Europe, à Cambridge cette fois, et c'est en anglais même si
plusieurs compatriotes seront dans l'assistance.

Si vous n'avez pas encore jeté un œil au 
[programme](http://www.char11.org/schedule), je vous encourage à le
faire. Même si vous n'aviez pas prévu de venir… parce qu'il y a de quoi vous
faire changer d'avis !

Il est déjà difficile de suivre les 
[listes de diffusions PostgreSQL](http://archives.postgresql.org/) en
anglais, pour une simple question de temps, mais parfois la barrière de la
langue peut également jouer. Alors si vous n'aviez pas bien suivi, je me
permets de préciser qui sont les principaux intervenants à cette conférence.

***Jan Wieck*** assure la première intervention avec un rétrospectif des solutions
de réplication pour PostgreSQL. Il a initié 
[Slony](http://slony.info/) et continue d'être très
actif dans son architecture et son développement.

***Greg Smith***, un collègue chez 
[2ndQuadrant](http://www.2ndquadrant.us/), est monsieur performances « bas
niveau » : sa spécialité est de tirer le meilleur de votre matériel, de
votre configuration serveur, de PostgreSQL lui-même, et des requêtes que
vous lui soumettez. Son livre 
[PostgreSQL High Performance](http://www.2ndquadrant.com/books/postgresql-9-0-high-performance/) est un
incontournable, à ce titre 
[traduit en français](http://blog.guillaume.lelarge.info/index.php/post/2011/05/01/%C2%AB-Bases-de-donn%C3%A9es-PostgreSQL,-Gestion-des-performances-%C2%BB).

Nous avons ensuite 
***Magnus Hagander*** qui a rejoint récemment la 
*core team*
(l'organisation centrale du projet), et qui contribue depuis plus de 10 ans
au code de PostgreSQL.

***Simon Riggs***, lui aussi un de 
[nos collègues](http://www.2ndquadrant.com/about/#riggs), a réalisé le 
*PITR*, l'archivage
des journaux de transactions, la réplication asynchrone et pour la prochaine
version de PostgreSQL, la réplication synchrone.

***Hannu Krosing*** (devinez 
[où](http://www.2ndquadrant.com/) il travaille ?) a conçu l'architecture (et les
outils) qui permettent à 
[Skype](http://www.skype.com/) d'annoncer une « scalability » infinie, en
tout cas annoncée pour supporter jusqu'à 
[1 milliard d'utilisateurs](http://highscalability.com/skype-plans-postgresql-scale-1-billion-users).

***Koichi Suzuki*** dirige les efforts du produit prometteur 
[PostgreS-XC](http://postgres-xc.sourceforge.net/), un bel
exemple de collaboration entre différents acteurs du marché, ici
[EnterpriseDB](http://www.enterprisedb.com/) et 
[NTT Open Source Software Center](https://www.oss.ecl.ntt.co.jp/ossc/). Ce qui montre une fois de
plus que l'
[Open Source](http://fr.wikipedia.org/wiki/Open_source) est solidement ancré dans entreprises commerciales.

Bien sûr, Cédric et moi-même, de la partie française de 
[2ndQuadrant](http://www.2ndquadrant.fr/), serons
de la partie. Nous interviendrons sur des sujets que nous connaissons bien
pour avoir participé à leur développement et pour les déployer et les
maintenir en production, 
[repmgr](http://projects.2ndquadrant.com/repmgr) et 
[Londiste](http://wiki.postgresql.org/wiki/Londiste_Tutorial).

Et je passe sur d'autres profils, dont les sujets ne serront pas moins
intéressants. Bref, si 
*réplication* et 
*cluster* sont des thèmes que vous
voulez conjuguer avec PostgreSQL, c'est l'endroit où passer le début de la
semaine prochaine !
