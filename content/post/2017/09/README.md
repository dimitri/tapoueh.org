# On JSON and SQL

See the articles:

  http://tapoueh.org/blog/2017/09/on-json-and-sql/
  http://tapoueh.org/blog/2017/10/set-returning-fonctions-and-postgresql-10/

To use the files:

~~~ bash
$ createdb magic
$ psql -d magic -a -f magic.create.sql
$ PGOST=... PGPORT=... PGDATABASE=magic python3 magic.py
$ psql -d magic -a -f magic.sql
~~~

Details at the blog post articles.
