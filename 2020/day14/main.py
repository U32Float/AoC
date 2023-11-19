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

pat = re.compile(r"mem\[(\d+)\] = (\d+)")
program = []
mask = None
values = []
for line in lines:
    if line[:4] == "mask":
        on = int("".join([str(int(l == "1")) for l in list(line[7:])]), 2)
        off = int("".join([str(int(l == "0")) for l in list(line[7:])]), 2)
        floating = [l == "X" for l in list(line[7:])]
        if mask is not None:
            program.append((mask, values))
        mask = (on, off, floating)
        values = []
    else:
        addr, val = pat.match(line).groups()
        values.append((int(addr), int(val)))
program.append((mask, values))

# ----------------------------------------------------------------------
# * Part 1

memory = {}
for (mask_on, mask_off, _), values in program:
    for addr, val in values:
        memory[addr] = (val | mask_on) & ~mask_off

print("Part 1:", sum(memory.values()))

# ----------------------------------------------------------------------
# * Part 2


def get_adresses(x, floating):
    if len(floating) == 0:
        return [x]
    if not floating[0]:
        return get_adresses(x, floating[1:])
    else:
        return get_adresses(x | 1 << len(floating) - 1, floating[1:]) + \
            get_adresses(x & ~(1 << len(floating) - 1), floating[1:])


memory = {}
for (mask_on, _, floating), values in program:
    for addr, val in values:
        addr |= mask_on
        for a in get_adresses(addr, floating):
            memory[a] = val


print("Part 2:", sum(memory.values()))
