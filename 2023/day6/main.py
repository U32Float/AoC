import sys

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

race = [int(line[9:].replace(' ', '')) for line in lines]

total = 0
t = race[0]
for v in range(t):
    if distance(t, v) > race[1]:
        total += 1

part2 = total
print("Part 2: " + str(part2))
