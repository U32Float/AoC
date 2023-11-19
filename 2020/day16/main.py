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

pat = re.compile(r"(\d+)-(\d+) or (\d+)-(\d+)")
fields = {line.split(":")[0]: pat.search(line).groups()
          for line in next(groupby(lines, lambda l: l == ""))[1]}

ticket = list(map(int, lines[lines.index("your ticket:") + 1].split(",")))
nearby = [list(map(int, line.split(",")))
          for line in lines[lines.index("nearby tickets:") + 1:]]


# ----------------------------------------------------------------------
# * Part 1

rate = 0
filtered = []
for t in nearby:
    valid = True
    for val in t:
        if not any(int(a) <= val <= int(b) or int(c) <= val <= int(d)
                   for a, b, c, d in fields.values()):
            rate += val
            valid = False
    if valid:
        filtered.append(t)


print("Part 1:", rate)

# ----------------------------------------------------------------------
# * Part 2

can_be = defaultdict(set)
cannot_be = defaultdict(set)
for t in filtered:
    for i, val in enumerate(t):
        for field, (a, b, c, d) in fields.items():
            if int(a) <= val <= int(b) or int(c) <= val <= int(d):
                can_be[i].add(field)
            else:
                cannot_be[i].add(field)


for i in range(len(fields)):
    for field in cannot_be[i]:
        can_be[i].discard(field)

t = 1
n = len(fields)
while n > 0:
    for i, fields in can_be.items():
        if len(fields) == 1:
            field = fields.pop()
            for j in range(len(can_be)):
                can_be[j].discard(field)
            if field.startswith("departure"):
                t *= ticket[i]
            n -= 1

print("Part 2:", t)
