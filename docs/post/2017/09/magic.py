#! /usr/bin/env python3

import psycopg2

if __name__ == '__main__':
    pgconn = psycopg2.connect("")
    curs = pgconn.cursor()

    allset = open('MagicAllSets.json').read()
    allset = allset.replace("'", "''")
    sql = "insert into magic.allsets(data) values('%s')" % allset

    curs.execute(sql)
    pgconn.commit()
    pgconn.close()
