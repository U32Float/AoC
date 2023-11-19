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

data = np.sort(list(map(int, lines)))

# ----------------------------------------------------------------------
# * Part 1

d = [data[0]] + np.diff(data).tolist() + [3]
c = Counter(d)
print("Part 1:", c[1] * c[3])


# ----------------------------------------------------------------------
# * Part 2

c = 1
i = 0
while i < len(d):
    t = 0
    for j in range(4):
        if d[i + j] == 3:
            break
        t += 1
    match t:
        case 2:
            c *= 2
        case 3:
            c *= 4
        case 4:
            c *= 7

    i += j + 1

print("Part 2:", c)
