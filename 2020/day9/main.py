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

N = 5 if file == "ex1" else 25

data = list(map(int, lines))


# ----------------------------------------------------------------------
# * Part 1

x = None
for i in range(N, len(data)):
    if all(data[i] - data[j] not in data[i - N: i] for j in range(i - N, i)):
        x = data[i]
        break

print("Part 1:", x)

# ----------------------------------------------------------------------
# * Part 2

for i in range(len(data)):
    for j in range(i + 1, len(data)):
        if sum(data[i:j]) == x:
            print("Part 2:", min(data[i:j]) + max(data[i:j]))
            exit()
