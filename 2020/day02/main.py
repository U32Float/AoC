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

pat = re.compile(r"(\d+)-(\d+) (\w): (\w+)")
P = [pat.match(line).groups() for line in lines]

# ----------------------------------------------------------------------
# * Part 1

c = 0
for a, b, d, p in P:
    if int(a) <= p.count(d) <= int(b):
        c += 1
        
print("Part 1:", c)

# ----------------------------------------------------------------------
# * Part 2

c = 0
for a, b, d, p in P:
    if (p[int(a)-1] == d) ^ (p[int(b)-1] == d):
        c += 1
        
print("Part 2:", c)