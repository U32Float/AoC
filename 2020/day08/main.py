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

instructions = [(line[:3], (-1 if line[4] == "-" else 1) * int(line[5:]))
                for line in lines]

# ----------------------------------------------------------------------
# * Part 1

pc = 0
acc = 0
V = set()
while pc not in V:
    match  instructions[pc]:
        case ("acc", v):
            acc += v
        case ("jmp", v):
            pc += v
            continue
        case ("nop", _):
            pass
    V.add(pc)
    pc += 1

print("Part 1:", acc)

# ----------------------------------------------------------------------
# * Part 2


def run(instructions, acc, pc, V):
    while pc < len(instructions):
        match  instructions[pc]:
            case ("acc", v):
                acc += v
            case ("jmp", v):
                pc += v
                continue
            case ("nop", _):
                pass
        if pc in V:
            return None
        V.add(pc)
        pc += 1
    return acc


pc = 0
acc = 0
V = set()
while pc < len(instructions):
    match  instructions[pc]:
        case ("acc", v):
            acc += v
        case ("jmp", v):
            I = deepcopy(instructions)
            I[pc] = ("nop", v)
            a = run(I, acc, pc, deepcopy(V))
            if a is not None:
                acc = a
                break
            pc += v
            continue
        case ("nop", v):
            I = deepcopy(instructions)
            I[pc] = ("jmp", v)
            a = run(I, acc, pc, deepcopy(V))
            if a is not None:
                acc = a
                break

    V.add(pc)
    pc += 1

print("Part 2:", acc)
