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

nums = list(map(int, lines[0].split(",")))

# ----------------------------------------------------------------------
# * Part 1

spoken = {n: i + 1 for i, n in enumerate(nums)}
last = nums[-1]
i = len(nums) + 1
while i <= 2020:
    if last in spoken:
        next = i - 1 - spoken[last]
    else:
        next = 0
    spoken[last] = i - 1
    last = next
    i += 1

print("Part 1:", last)


# ----------------------------------------------------------------------
# * Part 2

spoken = [-1] * 30000000
for i, n in enumerate(nums):
    spoken[n] = i + 1
last = nums[-1]
i = len(nums) + 1
while i <= 30000000:
    j = spoken[last]
    if j == -1:
        next = 0
    else:
        next = i - 1 - j
    spoken[last] = i - 1
    last = next
    i += 1

print("Part 2:", last)
