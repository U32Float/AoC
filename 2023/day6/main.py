import sys
import math

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

races = [[int(x) for x in line[9:].split()] for line in lines]
N = len(races[0])

# ----------------------------------------------------------------------
# * Part 1


def distance(t, v):
    return t*v - v**2


part1 = 1
for i in range(N):
    total = 0
    t = races[0][i]
    for v in range(t):
        if distance(t, v) > races[1][i]:
            total += 1
    part1 *= total

print("Part 1: " + str(part1))

# ----------------------------------------------------------------------
# * Part 2


def discriminant(t, d):
    return t**2 - 4 * d


def roots(t, d):
    d = discriminant(t, d)
    s = math.sqrt(abs(d))
    if d == 0:
        return [t / 2]
    else:
        return [(-t + s)/-2, (-t - s)/-2]


race = [int(line[9:].replace(' ', '')) for line in lines]
[lower, upper] = roots(race[0], race[1])

lower_bound = math.floor(lower)
if math.ceil(lower) > race[1]:
    lower_bound = math.ceil(lower)

upper_bound = math.floor(upper)
if math.ceil(upper) > race[1]:
    upper_bound = math.ceil(upper)

print("Part 2: " + str(upper_bound - lower_bound))
