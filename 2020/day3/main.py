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

M = np.array([[l == "#" for l in line]for line in lines])

# ----------------------------------------------------------------------
# * Part 1

p = (0, 0)
t = 0
while p[0] < M.shape[0]:
    t += M[p]
    p = (p[0] + 1, (p[1] + 3) % M.shape[1]) 

print("Part 1:", t)

# ----------------------------------------------------------------------
# * Part 2

m = 1
for dy, dx in [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]:
    p = (0, 0)
    t = 0
    while p[0] < M.shape[0]:
        t += M[p]
        p = (p[0] + dy, (p[1] + dx) % M.shape[1]) 
    m *= t

print("Part 2:", m)