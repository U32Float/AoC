import re
import sys
import itertools
from collections import defaultdict

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

seeds = [int(s) for s in lines[0][6:].split()]

pat = re.compile(r"(\w+)-to-(\w+)")

almanac = {}
for f, group in itertools.groupby(lines[1:], lambda l: l == ""):
    if f:
        continue
    group = list(group)
    categories = pat.search(group[0]).groups()

    almanac[categories] = []

    for r in group[1:]:
        [d, s, n] = r.split()
        almanac[categories].append([int(d), int(s), int(n)])

PATH = ["seed", "soil", "fertilizer", "water",
        "light", "temperature", "humidity", "location"]

# ----------------------------------------------------------------------
# * Part 1

nums = []
for seed in seeds:
    val = seed
    for i in range(len(PATH) - 1):
        for r in almanac[(PATH[i], PATH[i+1])]:
            if val in range(r[1], r[1] + r[2]):
                val = r[0] + val - r[1]
                break

    nums.append(val)

print("Part 1: " + str(min(nums)))

# ----------------------------------------------------------------------
# * Part 2


def intersect_range(r1, r2):
    return range(max(r1.start, r2.start), min(r1.stop, r2.stop))


def map_range(r, dest):
    dest.sort(key=lambda x: x[1])

    ranges = []
    for [d, s, n] in dest:
        intersection = intersect_range(r, range(s, s + n))
        if intersection.stop <= intersection.start:
            continue
        ranges.append(range(d + intersection.start -
                      s, d + intersection.stop - s))
        if r.start != intersection.start:
            ranges.append(range(r.start, intersection.start))
        r = range(intersection.stop, r.stop)

    if r.start != r.stop:
        ranges.append(r)

    return ranges


it = iter(seeds)
rs = []
for start in it:
    old_ranges = [range(start, start + next(it))]
    for i in range(len(PATH) - 1):
        new_ranges = []
        for ran in old_ranges:
            new_ranges += map_range(ran, almanac[(PATH[i], PATH[i+1])])
        old_ranges = new_ranges
    rs += old_ranges

print("Part 2: " + str(min([r.start for r in rs])))
