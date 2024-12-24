import sys
import numpy as np
from scipy.ndimage import generic_filter
from copy import deepcopy
import itertools
import math

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

schematic = np.mat([list(line) for line in lines])

# ----------------------------------------------------------------------
# * Part 1


def map_symbol(c: str):
    if c == '.':
        return -20
    elif c.isdigit():
        return -int(c) - 1
    else:
        return -30


def grow(region):
    mask = np.ma.masked_where(region == -30, region).mask
    if mask.sum() >= 1 and region[4] in range(-10, 0):
        return -region[4] - 1
    if region[4] in range(-10, 0) and (region[3] in range(0, 10) or region[5] in range(0, 10)):
        return -region[4] - 1
    return region[4]


def cleanup(x: int):
    if x in range(0, 10):
        return str(x)
    else:
        return '.'


def find_nums(schematic):
    old = schematic
    new = generic_filter(schematic, grow, size=3)
    while not np.array_equal(old, new):
        old = new
        new = generic_filter(old, grow, size=3)
    nums = np.vectorize(cleanup)(new)
    nums = [[int(num) for num in "".join(line).split('.') if num != ""]
            for line in nums]
    return list(itertools.chain(*nums))


mapped = np.vectorize(map_symbol)(schematic)
total = sum(find_nums(mapped))

print("Part 1: " + str(total))

# ----------------------------------------------------------------------
# * Part 2


def map_gear(c: str):
    if c.isdigit():
        return -int(c) - 1
    else:
        return -20


total = 0
mapped = np.vectorize(map_gear)(schematic)
for [r, c] in np.argwhere(schematic == '*'):
    m = deepcopy(mapped)
    m[r, c] = -30
    rows = []
    if r != 0:
        rows.append(r-1)
    rows.append(r)
    if r != schematic.shape[0] - 1:
        rows.append(r + 1)
    m = m[rows, :]
    nums = find_nums(m)
    if len(nums) == 2:
        gear_ratio = math.prod(nums)
        total += gear_ratio


print("Part 2: " + str(total))
