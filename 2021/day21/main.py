import re
from enum import Enum
from copy import deepcopy
from pprint import pprint
from functools import reduce, cmp_to_key
from itertools import groupby, combinations, permutations, product
from time import time
from queue import PriorityQueue
from math import prod, ceil, comb, perm
from collections import defaultdict, Counter
from dataclasses import dataclass
from operator import add, mul
from functools import lru_cache
from bisect import insort
import numpy as np
import string
import sys
from sys import exit

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

START1 = int(lines[0][28:])
START2 = int(lines[1][28:])

# ----------------------------------------------------------------------
# * Part 1

P1 = START1 - 1
P2 = START2 - 1
S1 = 0
S2 = 0
d = 0
r = 0

while True:
    P1 = (P1 + d * 3 + 6) % 10
    S1 += P1 + 1
    d = (d + 3) % 100
    r += 3
    if S1 >= 1000:
        print("Part 1: {}".format(r * S2))
        break

    P2 = (P2 + d * 3 + 6) % 10
    S2 += P2 + 1
    d = (d + 3) % 100
    r += 3
    if S2 >= 1000:
        print("Part 1: {}".format(r * S1))
        break


# ----------------------------------------------------------------------
# * Part 2

DICE = Counter(map(sum, product([1, 2, 3], repeat=3)))


class Universe:
    def __init__(self, start1, start2):
        self.P = [start1, start2]
        self.S = [0, 0]
        self.t = 0

    def branch(self):
        branches = []
        for d, n in DICE.items():
            b = deepcopy(self)
            b.P[b.t] = (b.P[b.t] + d) % 10
            b.S[b.t] += b.P[b.t] + 1
            b.t = not b.t
            branches.append((b, n))
        return branches

    def outcome(self):
        return next((i for i, s in enumerate(self.S) if s >= 21), None)

    def __hash__(self):
        return hash((self.P[0], self.P[1], self.S[0], self.S[1], self.t))

    def __eq__(self, other):
        return self.P == other.P and self.S == other.S and self.t == other.t

    def __repr__(self):
        return f"Universe({self.P}, {self.S}, {self.t})"


@lru_cache(maxsize=None)
def count(universe):
    out = universe.outcome()
    if out != None:
        w = [0]
        w.insert(out, 1)
        return w

    w = [0, 0]
    for b, n in universe.branch():
        w = list(map(add, w, map(lambda x: x * n, count(b))))

    return w


root = Universe(START1 - 1, START2 - 1)
W = count(root)
print("Part 2: {}".format(max(W)))
