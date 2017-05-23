+++
date = "2011-08-12T11:01:00.000000+02:00"
title = "Champs statiques & pgloader"
tags = ["PostgreSQLFr", "pgloader"]
categories = ["Projects","pgloader"]
thumbnailImage = "/img/old/article2.gif"
thumbnailImagePosition = "left"
coverImage = "/img/old/article2.gif"
coverSize = "partial"
coverMeta = "out"
aliases = ["/blog/2011/08/12-pgloader-constantes",
           "/blog/2011/08/12-pgloader-constantes.html"]
+++

Dans la série de nos articles sur 
[pgloader](http://tapoueh.org/tags/pgloader.html), l'article du jour décrit
[comment insérer des valeurs constantes](http://tapoueh.org/blog/2011/08/12-pgloader-udc.html) (absentes du fichier de données)
pendant le chargement.  Cela permet par exemple d'ajouter un champ
« origine », qui dépend typiquement de la chaîne de production des données
et se retrouve souvent dans le nom du fichier de données lui-même.
