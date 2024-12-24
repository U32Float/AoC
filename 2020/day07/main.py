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

pat = re.compile(r"(\w+ \w+)")
pat2 = re.compile(r"(\d+) (\w+ \w+)|no other bags")
R = {pat.match(line).groups()[0]: {m[1]: int(m[0])
                                   for m in pat2.findall(line) if m[1] != ""}
     for line in lines}

# ----------------------------------------------------------------------
# * Part 1


@lru_cache(maxsize=None)
def contains_gold(bag):
    if bag == "shiny gold":
        return True
    for b in R[bag]:
        if contains_gold(b):
            return True
    return False


print("Part 1:", sum(contains_gold(b) for b in R) - 1)

# ----------------------------------------------------------------------
# * Part 2


@lru_cache(maxsize=None)
def count(bag):
    return sum(R[bag][b] * (1 + count(b)) for b in R[bag])


print("Part 2:", count("shiny gold"))
