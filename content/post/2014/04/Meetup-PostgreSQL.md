+++
date = "2014-04-17T13:28:00.000000+02:00"
title = "Meetup PostgreSQL à Paris"
tags = ["PostgreSQLFr", "Meetup", "Conferences"]
categories = ["Conferences","Meetup"]
thumbnailImage = "/img/old/the-eiffel-tower-and-the-elephant-by-fremiet-jules-ernest-renoux.640.jpg"
thumbnailImagePosition = "left"
coverImage = "/img/old/the-eiffel-tower-and-the-elephant-by-fremiet-jules-ernest-renoux.640.jpg"
coverSize = "partial"
coverMeta = "in"
aliases = ["/blog/2014/04/17-Meetup-PostgreSQL",
           "/blog/2014/04/17-Meetup-PostgreSQL.html"]
+++

Hier soir se déroulait le troisième 
[Meetup PostgreSQL à Paris](http://www.meetup.com/PostgreSQL-User-Group-Paris/), et je crois
pouvoir dire que tous les participants étaient ravis de cette édition.

Malheureusement, quelques uns sont restés bloqués du mauvais côté de la
porte hier soir, j'en suis navré. Nous essayerons d'étendre nos horaires
d'accueil lors des prochaines rencontres, dans la mesure du possible.


## Les sponsors de la soirée

Un grand merci à nos sponsors : hier soir 
[eNovance](https://www.enovance.com/) nous accueillait dans une
superbe salle de conférence et 
[Hegoa](http://www.hegoa-recrut.com/) s'est chargée de mettre à disposition
des participants un buffet très apprécié.

Si vous désirez intervenir auprès de notre organisation en tant que sponsor
ou bien en tant que conférencier, n'hésitez pas à m'envoyer un 
*email* afin de
nous organiser au mieux !


## Annonce Officielle du Ministère de l'Intérieur

***Vincent Laborie*** a eu l'opportunité hier soir de faire la première annonce
officielle de l'utilisation de PostgreSQL en production au sein des Systèmes
d'Information de la Sécurité Intérieure, c'est à dire dans les applications
critiques déployées au sein du Ministère de l'Intérieur et dont les
utilisateurs sont nos policiers et gendarmes.


## OpenStack et PostgreSQL

[Julien Danjou](http://julien.danjou.info/) nous a ensuite présenté l'utilisation faite par 
[OpenStack](https://www.openstack.org/) de
[PostgreSQL](http://www.postgresql.org/), et nous retiendrons qu'il reste beaucoup de progrès à faire en
la matière, à commencer par une intégration véritable des tests unitaires.

Une fois de plus, une analyse faite avec assez de recul montre que
l'utilisation d'un ORM reste un problème car il impose un nivellement par le
bas des fonctionnalités exploitées dans les applications. La 
*portabilité* du
SGBD cible ne s'obtient qu'à un coût exhorbitant.

<center>
<div class="figure dim-margin">
  <a href="http://julien.danjou.info/books/the-hacker-guide-to-python">
    <img src="/img/old/the-hacker-guide-to-python.png">
  </a>
</div>
</center>

<center>*Learn everything you need to build a successful Python project*</center>

Si vous voulez approfondir le sujet, je détaille mon point de vue sur les
problèmes d'intégration liés aux ORM dans l'excellent livre
[The Hacker's Guide to Python](http://julien.danjou.info/books/the-hacker-guide-to-python) où 
**Julien** me fait l'honneur d'une interview.


## Tsung et PostgreSQL

[Rodolphe Quiédeville](http://blog.rodolphe.quiedeville.org/) nous a ensuite présenté le merveilleux outil 
[Tsung](http://tsung.erlang-projects.org/) dans
un cas d'utilisation convainquant. Ses slides sont déjà disponibles en
ligne : 
[Un Tsung vaut mieux que 2 "croisons les doigts"](http://www.slideshare.net/RodolpheQuideville/tsung-meetuppostgresqlparisavr2014).

<center>
<div class="figure dim-margin">
  <a href="http://tsung.erlang-projects.org/">
    <img src="/img/old/tsung_logo_non_officiel.png">
  </a>
</div>
</center>

<center>*Tsung is an open-source multi-protocol distributed load testing tool*</center>


## Prochaine rencontre

Les détails sont encore à valider, à priori la prochaine rencontre des
utilisateurs de PostgreSQL à Paris se tiendra au moins de juin, un mois déjà
chargé en activité PostgreSQL avec non seulement le 
[PGDay France](http://pgday.fr/) à Toulon
les 5 et 6 juin, mais également une belle participation de notre communauté
à la conférence 
[PHP Tour](http://afup.org/pages/phptourlyon2014/) qui se tiendra à Lyon les 23 et 24 Juin 2014.

Note : c'est la rencontre qui est située à Paris, les utilisateurs de
PostgreSQL sont les bienvenus d'où qu'ils viennent !
