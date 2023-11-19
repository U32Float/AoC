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
from scipy.ndimage import generic_filter

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

NB_EVEN = {"w": (0, -2),
           "nw": (1, -1),
           "ne": (1, 1),
           "e": (0, 2),
           "se": (0, 1),
           "sw": (0, -1)}

NB_ODD = {"w": (0, -2),
          "nw": (0, -1),
          "ne": (0, 1),
          "e": (0, 2),
          "se": (-1, 1),
          "sw": (-1, -1)}

# ----------------------------------------------------------------------
# * Part 1

flipped = defaultdict(lambda: False)
minp = np.array([1000000, 1000000], dtype=int)
maxp = np.array([-1000000, -1000000], dtype=int)

for line in lines:
    pos = np.array([0, 0], dtype=int)

    pat = r"(nw)|(ne)|(sw)|(se)|(w)|(e)"
    for d in re.finditer(pat, line):
        if pos[1] % 2 == 0:
            pos += np.array(NB_EVEN[d.group()])
        else:
            pos += np.array(NB_ODD[d.group()])

    flipped[(pos[0], pos[1])] ^= True

    minp[0] = min(minp[0], pos[0])
    minp[1] = min(minp[1], pos[1])
    maxp[0] = max(maxp[0], pos[0])
    maxp[1] = max(maxp[1], pos[1])

print("Part 1: {}".format(len([f for _, f in flipped.items() if f == True])))

# ----------------------------------------------------------------------
# * Part 2

def debug(floor):
    for row in floor:
        print(" ".join(["1" if b else "0" for b in row]))

def flip(floor, days):
    for _ in range(days):
        floor = np.pad(floor, 4, constant_values=False)
        (m, n) = floor.shape
        flipped = floor.copy()
        for i in range(2, n-2):
            for j in range(2, m-2):
                dirs = NB_EVEN.values() if j % 2 == 0 else NB_ODD.values()

                adj = np.sum([floor[j + y][i + x] for x, y in dirs])

                if floor[j][i] == True:
                    if adj == 0 or adj > 2:
                        flipped[j][i] = False
                else:
                    if adj == 2:
                        flipped[j][i] = True
        floor = flipped
        debug(floor)

    return floor


size = np.array(maxp - minp + 1, dtype=int)
floor = np.zeros((size[1], size[0]), dtype=bool)
for x, y in flipped.keys():
    floor[y - minp[1]][x - minp[0]] = True


if minp[1] % 2 == 1:
    floor = np.pad(floor, 1, constant_values=False)
    
pprint(flipped)
debug(floor)

print("Part 2: {}", np.sum(flip(floor, 2)))
