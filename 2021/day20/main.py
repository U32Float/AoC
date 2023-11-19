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
from scipy.ndimage import generic_filter

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

algo = np.array(
    list(map(lambda l: True if l == '#' else False, list(lines[0]))))
img = np.array([list(map(lambda l: True if l == '#' else False, list(line)))
               for line in lines[2:]])

# ----------------------------------------------------------------------
# * Part 1


def kernel(X):
    binary = "".join([str(int(x)) for x in X])
    return algo[int(binary, 2)]


def enhance(img, step):
    ref = np.pad(img, 1, constant_values=(step % 2) and algo[0])
    return generic_filter(ref, kernel, size=3)


out = enhance(enhance(img, 0), 1)
print("Part 1: {}".format(out.sum()))

# ----------------------------------------------------------------------
# * Part 2

out = img
for i in range(50):
    out = enhance(out, i)

print("Part 2: {}".format(out.sum()))
