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

instructions = [(line[0], int(line[1:])) for line in lines]

# ----------------------------------------------------------------------
# * Part 1

x, y = 0, 0
dir = 0
for action, v in instructions:
    if action == "N":
        y += v
    elif action == "S":
        y -= v
    elif action == "E":
        x += v
    elif action == "W":
        x -= v
    elif action == "L":
        dir = (dir - v // 90) % 4
    elif action == "R":
        dir = (dir + v // 90) % 4
    elif action == "F":
        if dir == 0:
            x += v
        elif dir == 1:
            y -= v
        elif dir == 2:
            x -= v
        elif dir == 3:
            y += v

print("Part 1:", abs(x) + abs(y))

# ----------------------------------------------------------------------
# * Part 2

x, y = 0, 0
wx, wy = 10, 1
for action, v in instructions:
    if action == "N":
        wy += v
    elif action == "S":
        wy -= v
    elif action == "E":
        wx += v
    elif action == "W":
        wx -= v
    elif action == "L":
        for _ in range(v // 90):
            wx, wy = -wy, wx
    elif action == "R":
        for _ in range(v // 90):
            wx, wy = wy, -wx
    elif action == "F":
        x += wx * v
        y += wy * v

print("Part 2:", abs(x) + abs(y))
