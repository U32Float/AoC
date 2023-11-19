import re
from enum import Enum
from copy import deepcopy
from pprint import pprint
from functools import reduce, cmp_to_key
from itertools import groupby, combinations, permutations
from time import time
from queue import PriorityQueue
from math import prod, ceil, comb, perm
from collections import defaultdict, Counter
from dataclasses import dataclass
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

wires = [line.split(",") for line in lines]

# ----------------------------------------------------------------------
# * Part 1

path = [set(), set()]
cost = [defaultdict(lambda: np.inf), defaultdict(lambda: np.inf)]

DIRS = {"R": (1, 0), "L": (-1, 0), "U": (0, -1), "D": (0, 1)}

for j, wire in enumerate(wires):
    pos = (0, 0)
    c = 1
    for dir in wire:
        dx, dy = DIRS[dir[0]]
        l = int(dir[1:])
        for i in range(1, l+1):
            p = (pos[0] + i*dx, pos[1] + i*dy)
            path[j].add(p)
            cost[j][p] = min(cost[j][p], c)
            c += 1

        pos = (pos[0] + l*dx, pos[1] + l*dy)

intersections = list(path[0].intersection(path[1]))

print("Part 1: {}".format(
    sorted([abs(i[0]) + abs(i[1]) for i in intersections])[0]))

# ----------------------------------------------------------------------
# * Part 2

print("Part 2: {}".format(
    sorted([cost[0][i] + cost[1][i] for i in intersections])[0]))
