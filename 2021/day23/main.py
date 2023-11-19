import re
from enum import Enum
from copy import deepcopy
from pprint import pprint
from functools import reduce, cmp_to_key
from itertools import groupby, combinations, permutations, takewhile
from time import time
from math import prod, ceil, comb, perm
from collections import defaultdict, Counter
from dataclasses import dataclass
from heapq import heappush, heappop
from bisect import insort
from operator import add, mul
from functools import lru_cache
import numpy as np
import string
import sys
from sys import exit

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

energy = {"A": 1, "B": 10, "C": 100, "D": 1000}

amphipods = [(a, (1, i+2)) for i,
             a in enumerate(lines[2][3:-3]) if a in "ABCD"]
amphipods += [(a, (2, i+2)) for i,
              a in enumerate(lines[3][3:-1]) if a in "ABCD"]

rooms = {a: (ord(a)-64)*2 for a in "ABCD"}

# ----------------------------------------------------------------------
# * Part 1


class State:
    def __init__(self, amphipods, size):
        self.amphipods = amphipods
        self.burrow = np.full((1 + size, 11), "#")
        self.burrow[0, :] = "."
        self.burrow[1:(1+size), [2, 4, 6, 8]] = "."
        for a, (y, x) in amphipods:
            self.burrow[y, x] = a
        self.size = size
        self.cost = 0

    def branch(self):
        branches = []
        for i in range(self.size * 4):
            a, p = self.amphipods[i]
            Q = self.moves(i)
            for q, n in Q:
                b = deepcopy(self)
                b.amphipods[i] = (a, q)
                b.burrow[p] = "."
                b.burrow[q] = a
                b.cost += n * energy[a]
                branches.append(b)
        return branches

    def moves(self, i):
        a, p = self.amphipods[i]

        if p[0] == 0:
            Y = list(takewhile(lambda y: self.burrow[y, rooms[a]] == ".", range(
                1, self.size+1)))
            if len(Y) > 0 and all(self.burrow[y, rooms[a]] == a for y in range(Y[-1]+1, self.size+1)):
                if all(self.burrow[0, x] == "." for x in range(min(p[1], rooms[a]), max(p[1], rooms[a])+1) if x != p[1]):
                    return [((Y[-1], rooms[a]), abs(p[1] - rooms[a]) + len(Y))]
            else:
                return []

        if any(self.burrow[y, p[1]] != "." for y in range(p[0]-1, 0, -1)):
            return []

        if p[1] == rooms[a]:
            if all(self.burrow[y, p[1]] == a for y in range(p[0]+1, self.size+1)):
                return []
        else:
            Y = list(takewhile(lambda y: self.burrow[y, rooms[a]] == ".", range(
                1, self.size+1)))
            if len(Y) > 0 and all(self.burrow[y, rooms[a]] == a for y in range(Y[-1]+1, self.size+1)):
                if all(self.burrow[0, x] == "." for x in range(min(p[1], rooms[a]), max(p[1], rooms[a])+1) if x != p[1]):
                    return [((Y[-1], rooms[a]), abs(p[1] - rooms[a]) + len(Y) + p[0])]

        J = [j for j in range(11) if j not in [2, 4, 6, 8]
             and self.burrow[0, j] == "."]
        M = []
        for j in J:
            if all(self.burrow[0, k] == "." for k in range(min(p[1], j), max(p[1], j)+1)):
                M.append(((0, j), abs(p[1] - j) + p[0]))
        return M

    def heuristic(self):
        h = self.cost
        for a, p in self.amphipods:
            d = abs(rooms[a] - p[1])
            if d > 0:
                h += (d + 1 + p[0]) * energy[a]
        return h

    def final(self):
        return all(rooms[a] == p[1] for a, p in self.amphipods)

    def __lt__(self, other):
        return self.cost < other.cost

    def __hash__(self):
        return hash(str(self.amphipods))

    def __repr__(self):
        s = "#" * 13 + "\n"
        s += "#" + "".join(self.burrow[0, :]) + "#\n"
        s += "#" + "".join(self.burrow[1, :]) + "#\n"
        for i in range(self.size - 1):
            s += "  " + "".join(self.burrow[2+i, 1:-1]) + "  \n"
        s += "  " + "#" * 9 + "  "
        return s


def solve(amphiphods, size):
    START = State(amphipods, size)

    Q = [START]
    V = {}
    minc = np.inf

    while Q:
        state = heappop(Q)

        if state.final():
            minc = min(minc, state.cost)
            continue

        f = state.heuristic()
        if f >= minc:
            continue

        h = hash(state)
        if h in V and state.cost >= V[h]:
            continue
        V[h] = state.cost

        for b in state.branch():
            heappush(Q, b)

    return minc


print("Part 1: {}".format(solve(amphipods, 2)))

# ----------------------------------------------------------------------
# * Part 2

for i in range(8):
    a, p = amphipods[i]
    if p[0] == 2:
        amphipods[i] = (a, (4, p[1]))

amphipods += [
    ("D", (2, 2)),
    ("D", (3, 2)),
    ("C", (2, 4)),
    ("B", (3, 4)),
    ("B", (2, 6)),
    ("A", (3, 6)),
    ("A", (2, 8)),
    ("C", (3, 8)),
]

print("Part 2: {}".format(solve(amphipods, 4)))
