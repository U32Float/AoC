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

M = [list(l) for l in lines]
E = np.array([[s == ">" for s in m] for m in M])
S = np.array([[s == "v" for s in m] for m in M])


def debug():
    r, c = E.shape
    for i in range(r):
        for j in range(c):
            if E[i, j]:
                print(">", end="")
            elif S[i, j]:
                print("v", end="")
            else:
                print(".", end="")
        print()


# ----------------------------------------------------------------------
# * Part 1

i = 1
while True:
    FE = np.roll(E, -1, axis=1)
    FS = np.roll(S, -1, axis=1)
    B = np.roll(E, 1, axis=1)
    F = FE | FS
    E = (E & F) | ((~E & ~S) & B)
    m = np.sum(~F & E)

    FE = np.roll(E, -1, axis=0)
    FS = np.roll(S, -1, axis=0)
    B = np.roll(S, 1, axis=0)
    F = FE | FS
    S = (S & F) | ((~E & ~S) & B)
    m += np.sum(~F & S)

    if m == 0:
        break

    i += 1


print("Part 1: {}".format(i + 1))
