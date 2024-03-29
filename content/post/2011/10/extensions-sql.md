+++
date = "2011-10-31T14:22:00.000000+01:00"
title = "Extensions en simple SQL"
tags = ["PostgreSQLfr", "Extensions"]
categories = ["PostgreSQL","PostgreSQLFr"]
thumbnailImage = "/img/old/using-extensions-10.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/using-extensions-10.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/10/31-extensions-sql",
           "/blog/2011/10/31-extensions-sql.html"]
+++

La 
[conférence européenne à Amsterdam](http://2011.pgconf.eu/) était un très bon évènement de la
communauté, avec une organisation impeccable dans un hôtel accueillant. J'ai
eu le plaisir d'y parler des extensions et de leur usage dans le cadre du
développement applicatif « interne », sous le titre
[Extensions are good for business logic](http://www.postgresql.eu/events/schedule/pgconfeu2011/session/138-extensions-are-good-for-business-logic/).

<center>
<div class="figure dim-margin">
  <a href="http://wiki.postgresql.org/images/f/f1/Using-extensions.pdf">
    <img src="/img/old/using-extensions-10.png">
  </a>
</div>
</center>

L'idée de ma présentation, que la plupart d'entre vous a loupé je suppose
(en tout cas je n'avais qu'une petite poignée de français dans la salle, et
j'espère avoir des lecteurs qui n'étaient pas à Amsterdam), l'idée est
d'utiliser les mécanismes offerts par les extensions afin de maintenir le
code 
`PL` que vous utilisez en production.

Il s'agit la plupart du temps de procédures qui implémentent une partie de
la logique métier de vos applications, mais si proche des données que cela
termine en base directement : c'est une bonne chose, en particulier depuis
*PostgreSQL 9.1*. Cette version propose en effet une gestion assez complète
des extensions.

Il s'agit de réaliser un 
*empaquetage* de vos procédures en suivant la
documentation en ligne et son chapitre
[35.15. Empaqueter des objets dans une extension](http://docs.postgresqlfr.org/9.1/extend-extensions.html). Une fois cela fait, il est
alors possible de déployer votre ensemble de procédure stockée avec la
commande 
`CREATE EXTENSION mesprocs;`, et ensuite la commande 
`psql` 
`\dx` vous
permet de lister les extensions installées et leur numéro de version.

Les mises à jours sont également gérées avec une commande SQL dédiée, il
s'agit alors de 
`ALTER EXTENSION mesprocs UPDATE [TO version];`. Il suffit de
fournir des scripts intermédiaires nommés par exemple 
`mesprocs--1.0--1.1.sql`
et 
`mesprocs--1.1--1.2.sql` et PostgreSQL saura comment passer de 
`1.0` à 
`1.1`.

Voilà, vous savez presque tout de ma présentation à Amsterdam et vous pouvez
retrouver le reste sur le support proposé en début d'article. Bien sûr je
n'ai pas reproduit ici les questions intéressantes qui m'ont été posées, une
bonne partie d'entre elles sont venues enrichir ma liste de Noël pour les
extensions. Si vous voulez être sûr de trouver cela sous votre sapin,
cependant, le meilleur moyen est encore de m'en parler : sponsoriser les
développement Open Source est une belle démarche :)
