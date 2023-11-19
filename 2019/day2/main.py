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

CODE = [int(instr) for instr in lines[0].split(",")]

# ----------------------------------------------------------------------
# * Part 1


def STM(code):
    pc = 0
    while True:
        match code[pc]:
            case 1:
                code[code[pc+3]] = code[code[pc+1]] + code[code[pc+2]]
            case 2:
                code[code[pc+3]] = code[code[pc+1]] * code[code[pc+2]]
            case 99:
                break
        pc += 4
    return code[0]


code = deepcopy(CODE)
code[1] = 12
code[2] = 2
print("Part 1: {}".format(STM(code)))

# ----------------------------------------------------------------------
# * Part 2

OUTPUT = 19690720

for i in range(100):
    for j in range(100):
        code = CODE.copy()
        code[1] = i
        code[2] = j

        if STM(code) == OUTPUT:
            print("Part 2: {}".format(i * 100 + j))
            exit()
