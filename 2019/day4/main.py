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

r = lines[0].split("-")
RANGE = range(int(r[0]), int(r[1]))


# ----------------------------------------------------------------------
# * Part 1

def rule1(p):
    for i in range(1, len(p)):
        if p[i] == p[i-1]:
            return True
    return False


def rule2(p):
    for i in range(1, len(p)):
        if int(p[i]) < int(p[i-1]):
            return False
    return True


n = 0
for p in RANGE:
    p = str(p)
    if rule1(p) and rule2(p):
        n += 1

print("Part 1: {}".format(n))

# ----------------------------------------------------------------------
# * Part 2

def rule1_exact(p):
    count = [1]
    last = p[0]
    for d in p[1:]:
        if d == last:
            count[-1] += 1
        else:
            count.append(1)
            last = d
    return 2 in count
    

n = 0
for p in RANGE:
    p = str(p)
    if rule1_exact(p) and rule2(p):
        n += 1

print("Part 2: {}".format(n))