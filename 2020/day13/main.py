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

t = int(lines[0])

# ----------------------------------------------------------------------
# * Part 1

ids = list(map(int, filter(lambda x: x != "x", lines[1].split(","))))

print("Part 1:", prod(min(map(lambda x: (x - t % x, x), ids))))

# ----------------------------------------------------------------------
# * Part 2

ids = list(map(lambda x: int(x) if x != "x" else 1, lines[1].split(",")))
t = 0
dt = ids[0]
found = 1
while True:
    if all((t + i) % ids[i] == 0 for i in range(len(ids)) if ids[i] != "x"):
        break
    if all((t + i) % ids[i] == 0 for i in range(found + 1)):
        dt *= ids[found]
        found += 1

    t += dt

print("Part 2:", t)
