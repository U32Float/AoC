import aocpy as aoc
from aocpy import print_bools, print_mat
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
lines = aoc.get_data(day=25, year=2022, example=example)

# ----------------------------------------------------------------------
# * Parsing(/setup)

SNAFU = {
    2: "2",
    1: "1",
    0: "0",
    3: "=",
    4: "-",
}
DIGIT = {
    "2": 2,
    "1": 1,
    "0": 0,
    "-": -1,
    "=": -2,
}


def snafu(decimal):
    s = ""
    while decimal > 0:
        d = decimal % 5
        s += SNAFU[d]
        decimal //= 5
        decimal += 1 if d == 4 or d == 3 else 0
    return s[::-1]


def decimal(snafu):
    W = np.array([5**i for i in range(len(snafu))])[::-1]
    return int((W * np.array(list(map(DIGIT.get, snafu)))).sum())


# ----------------------------------------------------------------------
# * Part 1

total = sum(map(decimal, lines))
print(total)
pprint("Part 1: {}".format(snafu(total)))
