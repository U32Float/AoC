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

P = [",".join(map(lambda k: ",".join(k.split()), g))
     for f, g in groupby(lines, lambda l: l == "") if not f]
P = [["\"{}\":\"{}\"".format(f.split(":")[0], f.split(":")[1])
      for f in p.split(",")] for p in P]
P = list(map(eval, map(lambda p: "{" + ",".join(p) + "}", P)))

F = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

# ----------------------------------------------------------------------
# * Part 1

c = 0
for p in P:
    c += all(f in p for f in F)

print("Part 1:", c)

# ----------------------------------------------------------------------
# * Part 2

c = 0
for p in P:
    if all(f in p for f in F) and \
        len(p["byr"]) == 4 and 1920 <= int(p["byr"]) <= 2002 and \
        len(p["iyr"]) == 4 and 2010 <= int(p["iyr"]) <= 2020 and \
        len(p["eyr"]) == 4 and 2020 <= int(p["eyr"]) <= 2030 and \
            (p["hgt"][-2:] == "cm" and 150 <= int(p["hgt"][:-2]) <= 193 or
             p["hgt"][-2:] == "in" and 59 <= int(p["hgt"][:-2]) <= 76) and \
            re.match(r"^#[0-9a-f]{6}$", p["hcl"]) and \
            p["ecl"] in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] and \
            re.match(r"^\d{9}$", p["pid"]):
        c += 1


print("Part 2:", c)
