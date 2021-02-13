# shadows generators for eager functions
from functools import reduce

def lmap(f, x):
    return list(map(f, x))


def lfilter(f, x):
    return list(filter(f, x))


def count(f, x):
    return len([_ for _ in filter(f, x)])


def iter(f, x):
    list(map(f, x))
    return None
