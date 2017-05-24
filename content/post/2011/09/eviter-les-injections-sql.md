+++
date = "2011-09-07T11:36:00.000000+02:00"
title = "Éviter les injections SQL"
tags = ["PostgreSQLFr"]
categories = ["PostgreSQL","PostgreSQLFr"]
thumbnailImage = "/img/old/postgresqlfr-logo.png"
thumbnailImagePosition = "left"
coverImage = "/img/old/postgresqlfr-logo.png"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/09/07-eviter-les-injections-sql",
           "/blog/2011/09/07-eviter-les-injections-sql.html"]
+++

Nous avons parlé la dernière fois les règles d'
[échappement de chaînes](http://tapoueh.org/blog/2011/08/18-echappements-de-chaine.html) avec
PostgreSQL, et mentionné qu'utiliser ces techniques afin de protéger les
données insérées dans les requêtes SQL n'était pas une bonne idée dans la
mesure où PostgreSQL offre une fonctionnalité bien plus adaptée.

Nous faisons face ici à un problème de sécurité très bien décrit dans le
billet humoristique de 
[Little Boby Tables](http://xkcd.com/327/), dont je vous recommande la
lecture. L'idée est simple, la mise en place de contre mesure fourmille de
pièges subtils, à moins d'utiliser la solution décrite ci-après.

<center>[http://imgs.xkcd.com/comics/exploits_of_a_mom.png](http://imgs.xkcd.com/comics/exploits_of_a_mom.png)</center>

Lorsque l'on envoie une requête SQL à PostgreSQL, celle-ci contient
pêle-mêle un mélange de mots-clés SQL et de données utilisateurs. Dans la
requête 
`sqlSELECT colname FROM table WHERE pk = 1234;`
l'élément 
`1234` est une donnée fournie à PostgreSQL. Lorsque l'on utilise
d'autre types de données, on va parler de 
*litéral*, qui peut être ou non
*décoré*.  Un exemple ?

~~~
=# SELECT 'undecorated literal', pg_typeof('undecoreted literal'),
          date 'today', pg_typeof(date 'today');
      ?column?       | pg_typeof |    date    | pg_typeof 
---------------------+-----------+------------+-----------
 undecorated literal | unknown   | 2011-09-07 | date
(1 row)
~~~


Outre l'aspect types de données (un litéral non décoré est de type 
*unknown*
jusqu'à ce qu'une opération force son type, c'est ce qui permet d'avoir du
polymorphisme dans PostgreSQL), nous voyons ici que PostgreSQL doit faire la
différence entre le SQL lui-même et les paramètres qui le composent. Il sait
bien sûr faire cela, il suffit d'encadrer les valeurs dans des simples
guillemets ou bien d'utiliser la notation dite de 
[dollar quoting](http://docs.postgresqlfr.org/9.0/sql-syntax.html#sql-syntax-dollar-quoting). Mais si
l'on ne prend pas de précautions, l'utilisateur peut terminer la séquence
d'échappements depuis le champ de saisie du formulaire…

[libpq](http://docs.postgresql.fr/9.1/libpq.html) est la librairie standard cliente de PostgreSQL et fourni des 
*API* de
connexion et propose une fonction 
[PGexecParams](http://docs.postgresql.fr/9.1/libpq-exec.html#libpq-pqexecparams). Cette fonction expose un
mécanisme disponible dans le protocole de communication de PostgreSQL
lui-même : il est possible de faire parvenir le SQL et les données qu'il
contient dans deux parties différentes du messages plutôt que de les
mélanger. Ainsi, le serveur n'a plus du tout à deviner où commencent et où
terminent les données dans la requête, il lui suffit de regarder dans le
tableau séparé contenant les données quand il en a besoin.

Terminées les injections SQL !

Note : cette fonction est exposée dans la plupart des pilotes de connexion,
et même en PHP, dont la popularité et l'exposition me poussent à donner une
référence plus précise : utilisez 
[pg_query_params](http://fr2.php.net/manual/en/function.pg-query-params.php), son intérêt n'est pas
simplement syntaxique, il va jusque dans la définition des échanges de
données entre le client (votre code PHP) et le serveur (PostgreSQL).
