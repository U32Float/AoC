import re
import sys
import math

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

pat = re.compile(r"(\d+) (blue|green|red)")

games = [[pat.findall(sub) for sub in line.split(';')] for line in lines]

# ----------------------------------------------------------------------
# * Part 1

BAG = {
    "red": 12,
    "green": 13,
    "blue": 14
}


def check_sub(sub):
    return all([int(num) <= BAG[color] for num, color in sub])


total = sum([i + 1 for i, game in enumerate(games)
             if all([check_sub(sub) for sub in game])])
print("Part 1: " + str(total))


# ----------------------------------------------------------------------
# * Part 2

power = []
for game in games:
    MAX = {
        "red": 0,
        "green": 0,
        "blue": 0
    }
    for sub in game:
        for num, color in sub:
            MAX[color] = max(MAX[color], int(num))
    power.append(math.prod(MAX.values()))

total = sum(power)
print("Part 2: " + str(total))
