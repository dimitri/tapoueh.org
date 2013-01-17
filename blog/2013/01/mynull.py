# Author: Dimitri Fontaine <dimitri@2ndQuadrant.fr>
#
# pgloader mysql reformating module

def timestamp(reject, input):
    """ Reformat str as a PostgreSQL timestamp

    MySQL timestamps are ok this time:  2012-12-18 23:38:12
    But may contain the infamous all-zero date, where we want NULL.
    """
    if input == '0000-00-00 00:00:00':
        return None

    return input

def date(reject, input):
    """ date columns can also have '0000-00-00'"""
    if input == '0000-00-00':
        return None

    return input
