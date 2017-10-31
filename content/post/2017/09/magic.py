#! /usr/bin/env python3

import psycopg2

if __name__ == '__main__':
    pgconn = psycopg2.connect("")
    curs = pgconn.cursor()

    allset = open('AllSets.json').read()
    allset = allset.replace("'", "''")
    sql = "insert into magic.allsets(data) values(%s)"

    curs.execute(sql, (allset,))
    pgconn.commit()
    pgconn.close()
