import queue
import aocpy as aoc
from aocpy import print_bools, print_mat, add_tuple
import re
from enum import Enum
from copy import deepcopy
from pprint import pprint
from functools import reduce, cmp_to_key
from itertools import groupby, combinations, permutations
from time import time
from queue import PriorityQueue
from math import prod, ceil, comb, perm
from collections import defaultdict
from dataclasses import dataclass
from bisect import insort
import numpy as np
import string
import sys

example = sys.argv[1] == "ex" if len(sys.argv) > 1 else True
lines = aoc.get_data(day=24, year=2022, example=example)

# ----------------------------------------------------------------------
# * Parsing(/setup)

grid = [
    list(map(lambda l: "" if l == "." else l, list(line)))[1:-1] for line in lines[1:-1]
]
START = (-1, 0)
w = len(grid[0])
h = len(grid)
END = (h, w - 1)


DIR = {
    ">": (0, 1),
    "<": (0, -1),
    "^": (-1, 0),
    "v": (1, 0),
}

N = w * h + 1
B = [grid]
prev = deepcopy(grid)
for _ in range(w * h):
    new = [["" for _ in range(w)] for _ in range(h)]

    for r in range(h):
        for c in range(w):
            D = prev[r][c]
            for d in list(D):  # type: ignore
                y, x = add_tuple((r, c), DIR[d])
                y %= h
                x %= w
                new[y][x] += d

    B.append(deepcopy(new))  # type: ignore
    prev = new


class State:
    def __init__(self, t, start):
        self.t = t
        self.pos = start

    def branch(self):
        branches = []
        for dy, dx in DIR.values():
            y, x = (self.pos[0] + dy, self.pos[1] + dx)
            if y < -1 or y > h or x < 0 or x >= w:
                continue
            b = deepcopy(self)
            b.pos = (y, x)
            b.t += 1
            branches.append(b)
        b = deepcopy(self)
        b.t += 1
        branches.append(b)
        return branches

    def valid(self):
        if self.pos == START or self.pos == END:
            return True
        if self.pos[0] >= h or self.pos[0] == -1:
            return False
        r, c = self.pos
        return B[self.t % N][r][c] == ""

    def __hash__(self):
        return hash((self.t % N, self.pos))

    def __eq__(self, other):
        return hash(self) == hash(other)

    def __repr__(self):
        return f"State({self.t}, {self.pos}"


# ----------------------------------------------------------------------
# * Part 1


def solve(start, end, t):
    Q = [State(t, start)]
    V = set()

    while Q:
        state = Q.pop(0)

        if state in V:
            continue
        V.add(state)

        if state.pos == end:
            return state.t

        if state.valid():
            Q.extend(state.branch())


print("Part 1: {}".format(solve(START, END, 0)))

# ----------------------------------------------------------------------
# * Part 2

t1 = solve(START, END, 0)
t2 = solve(END, START, t1)
print("Part 2: {}".format(solve(START, END, t2)))
