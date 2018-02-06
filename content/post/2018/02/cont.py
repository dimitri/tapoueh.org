#! /usr/bin/env python3

import psycopg2
from collections import namedtuple

Record = namedtuple('Record', 'date days')
PGCONNSTRING = "dbname=appdev application_name=cont"


def cont():
    "Fetch data from the factbook table"

    conn = psycopg2.connect(PGCONNSTRING)
    curs = conn.cursor()
    sql = """
  SELECT date, dollars
    FROM factbook
   WHERE date is not null
ORDER BY date
"""
    curs.execute(sql)

    previous_date = None
    previous_dollars = None

    current = Record(date=None, days=0)
    best = Record(date=None, days=0)

    for date, dollars in curs.fetchall():
        if previous_dollars:
            if dollars > previous_dollars:
                current = Record(current.date, current.days + 1)
                if current.days > best.days:
                    best = Record(current.date, current.days)
            else:
                current = Record(date, 0)
        else:
            current = Record(date, 0)

        previous_date, previous_dollars = date, dollars

    return best


if __name__ == '__main__':
    rising = cont()

    print("Continuously rising days: %d days from %s" % (rising.days,
                                                         rising.date))
