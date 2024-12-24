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

R = [[l == "B" for l in line[:-3]] for line in lines]
C = [[l == "R" for l in line[-3:]] for line in lines]


# ----------------------------------------------------------------------
# * Part 1

maxid = 0
ids = []
for r, c in zip(R, C):
    l = 0
    u = 128
    for b in r:
        if b:
            l = (l + u) // 2
        else:
            u = (l + u) // 2
    row = l

    l = 0
    u = 8
    for b in c:
        if b:
            l = (l + u) // 2
        else:
            u = (l + u) // 2
    col = l
    id = row * 8 + col
    ids.append(id)
    maxid = max(maxid, id)

print("Part 1:", maxid)

# ----------------------------------------------------------------------
# * Part 2

ids.sort()
for i in range(len(ids) - 1):
    if ids[i] + 1 != ids[i + 1]:
        print("Part 2:", ids[i] + 1)