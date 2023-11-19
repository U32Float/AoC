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

layout = np.array([[l == "L" for l in list(line)] for line in lines])
h, w = layout.shape


def debug():
    for i in range(h):
        for j in range(w):
            if occupied[i][j]:
                print("#", end="")
            elif layout[i][j]:
                print("L", end="")
            else:
                print(".", end="")
        print()

# ----------------------------------------------------------------------
# * Part 1


occupied = np.zeros_like(layout, dtype=bool)

prev = None
while True:
    prev = deepcopy(occupied)
    N = np.roll(occupied, 1, axis=0)
    N[0, :] = False
    S = np.roll(occupied, -1, axis=0)
    S[-1, :] = False
    E = np.roll(occupied, 1, axis=1)
    E[:, 0] = False
    W = np.roll(occupied, -1, axis=1)
    W[:, -1] = False
    NE = np.roll(N, 1, axis=1)
    NE[:, 0] = False
    NW = np.roll(N, -1, axis=1)
    NW[:, -1] = False
    SE = np.roll(S, 1, axis=1)
    SE[:, 0] = False
    SW = np.roll(S, -1, axis=1)
    SW[:, -1] = False
    adj = np.add.reduce(
        list(map(lambda A: A.astype(int), [N, S, E, W, NE, NW, SE, SW])))
    I = (adj >= 4) & occupied & layout
    J = (adj == 0) & ~occupied & layout
    occupied[I] = False
    occupied[J] = True
    if np.sum(prev ^ occupied) == 0:
        break

print("Part 1: ", np.sum(occupied))

# ----------------------------------------------------------------------
# * Part 2

occupied = np.zeros_like(layout, dtype=bool)

A = [[[None for _ in range(w)] for _ in range(h)] for _ in range(8)]
for i in range(h):
    for j in range(w):
        if not layout[i][j]:
            continue
        for k, (di, dj) in enumerate([(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (-1, -1), (1, -1), (-1, 1)]):
            if di == 0 and dj == 0:
                continue
            ii = i + di
            jj = j + dj
            while 0 <= ii < h and 0 <= jj < w:
                if layout[ii][jj]:
                    A[k][i][j] = (ii, jj)
                    break
                ii += di
                jj += dj


prev = None
while True:
    prev = deepcopy(occupied)
    adj = np.zeros_like(occupied, dtype=int)
    for i in range(h):
        for j in range(w):
            for k in range(8):
                if A[k][i][j] is not None:
                    v, u = A[k][i][j]
                    adj[i][j] += int(occupied[v, u])

    I = (adj >= 5) & occupied & layout
    J = (adj == 0) & ~occupied & layout
    occupied[I] = False
    occupied[J] = True
    if np.sum(prev ^ occupied) == 0:
        break

print("Part 2: ", np.sum(occupied))
