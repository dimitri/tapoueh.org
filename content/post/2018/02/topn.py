#! /usr/bin/env python3

import psycopg2
import heapq
import sys

PGCONNSTRING = "dbname=appdev application_name=cont"


def top(n):
    "Fetch data from the factbook table"

    conn = psycopg2.connect(PGCONNSTRING)
    curs = conn.cursor()
    sql = """
  SELECT date, dollars
    FROM factbook
   WHERE date is not null
"""
    curs.execute(sql)

    topn = [(0, None) for i in range(n)]
    heapq.heapify(topn)

    for date, dollars in curs.fetchall():
        heapq.heappushpop(topn, (dollars, date))

    return topn


if __name__ == '__main__':
    n = int(sys.argv[1])
    topn = top(n)

    for dollars, date in heapq.nlargest(n, topn):
        print("%s: %s" % (date, dollars))
