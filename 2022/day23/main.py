import aocpy as aoc
from aocpy import print_bools, print_mat
import re
from enum import Enum
from copy import deepcopy
from pprint import pprint
from functools import reduce, cmp_to_key
from itertools import groupby, combinations, permutations
from time import time
from queue import PriorityQueue
from math import prod, ceil, comb, perm
from collections import defaultdict
from dataclasses import dataclass
from bisect import insort
import numpy as np
import string
import sys

example = sys.argv[1] == "ex" if len(sys.argv) > 1 else True
lines = aoc.get_data(day=23, year=2022, example=example)

# ----------------------------------------------------------------------
# * Parsing(/setup)

H = list(map(lambda l: [i for i, c in enumerate(l) if c == "#"], lines))
elves = sum([[(x, -y) for x in X] for y, X in enumerate(H)], [])
pos = {e: i for i, e in enumerate(elves)}

DIRS = [
    [(0, 1), (-1, 1), (1, 1)],
    [(0, -1), (-1, -1), (1, -1)],
    [(-1, 0), (-1, -1), (-1, 1)],
    [(1, 0), (1, -1), (1, 1)],
]

N8 = list(set(sum(DIRS, [])))


def debug(E, P):
    minx = min(x for x, _ in E)
    miny = min(y for _, y in E)
    maxx = max(x for x, _ in E)
    maxy = max(y for _, y in E)

    w = maxx - minx + 1
    h = maxy - miny + 1
    mat = [["." for _ in range(w)] for _ in range(h)]
    for x, y in E:
        mat[y - miny][x - minx] = "#"
    print_mat(mat[::-1])

    print(all(p == E[e] for p, e in P.items()))


def plan(i, d, E, P):
    x, y = E[i]
    if not any((x + dx, y + dy) in P for dx, dy in N8):
        return None
    for _ in range(4):
        if not any((x + dx, y + dy) in P for dx, dy in DIRS[d]):
            dx, dy = DIRS[d][0]
            return (x + dx, y + dy)
        d = (d + 1) % 4
    return None


# ----------------------------------------------------------------------
# * Part 1

E = deepcopy(elves)
P = deepcopy(pos)


d = 0
for _ in range(10):
    Q = defaultdict(lambda: [])
    for i in range(len(E)):
        q = plan(i, d, E, P)
        if q:
            Q[q].append(i)
    Q = {q: e for q, e in Q.items() if len(e) == 1}
    for q, e in Q.items():
        p = E[e[0]]
        del P[p]
        P[q] = e[0]
        E[e[0]] = q
    d = (d + 1) % 4

minx = min(x for x, _ in E)
miny = min(y for _, y in E)
maxx = max(x for x, _ in E)
maxy = max(y for _, y in E)

print((maxx - minx + 1) * (maxy - miny + 1) - len(E))


# ----------------------------------------------------------------------
# * Part 2

E = deepcopy(elves)
P = deepcopy(pos)

d = 0
r = 1
while True:
    Q = defaultdict(lambda: [])
    for i in range(len(E)):
        q = plan(i, d, E, P)
        if q:
            Q[q].append(i)
    Q = {q: e for q, e in Q.items() if len(e) == 1}
    if len(Q) == 0:
        print(r)
        break
    for q, e in Q.items():
        p = E[e[0]]
        del P[p]
        P[q] = e[0]
        E[e[0]] = q
    d = (d + 1) % 4
    r += 1
