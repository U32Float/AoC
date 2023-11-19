from copy import deepcopy
from itertools import groupby
import sys

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

decks = [list(map(int, list(g)[1:]))
         for f, g in groupby(lines, lambda l: l == "") if not f]

# ----------------------------------------------------------------------
# * Part 1

p1, p2 = deepcopy(decks)
while True:
    if len(p1) == 0 or len(p2) == 0:
        break
    a, b = p1.pop(0), p2.pop(0)
    if a > b:
        p1.extend([a, b])
    else:
        p2.extend([b, a])

winner = p1 if len(p1) > 0 else p2
print("Part 1:", sum((i + 1) * v for i, v in enumerate(winner[::-1])))

# ----------------------------------------------------------------------
# * Part 2


def recursive_combat(p1, p2):
    V = set()
    while True:
        if str((p1, p2)) in V:
            return (1, p1)
        V.add(str((p1, p2)))
        if len(p1) == 0:
            return (2, p2)
        if len(p2) == 0:
            return (1, p1)
        a, b = p1.pop(0), p2.pop(0)
        if len(p1) >= a and len(p2) >= b:
            winner, _ = recursive_combat(p1[:a], p2[:b])
            if winner == 1:
                p1.extend([a, b])
            else:
                p2.extend([b, a])
            continue
        if a > b:
            p1.extend([a, b])
        else:
            p2.extend([b, a])


_, deck = recursive_combat(decks[0], decks[1])
print("Part 2:", sum((i + 1) * v for i, v in enumerate(deck[::-1])))
