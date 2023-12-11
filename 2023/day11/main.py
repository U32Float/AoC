import sys
import numpy as np
from itertools import combinations

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

grid = np.mat([[c for c in line] for line in lines])
galaxies = [tuple(g) for g in np.argwhere(grid == '#')]

# ----------------------------------------------------------------------
# * Part 1

I = np.array([i for i, row in enumerate(grid) if np.all(row == '.')])
J = np.array([j for j, col in enumerate(grid.T) if np.all(col == '.')])

cosmic_expansion = [(r + sum(I < r), c + sum(J < c)) for r, c in galaxies]

total = sum([abs(g2[0] - g1[0]) + abs(g2[1] - g1[1])
             for g1, g2 in combinations(cosmic_expansion, 2)])

print("Part 1: " + str(total))

# ----------------------------------------------------------------------
# * Part 2

cosmic_expansion = [(r + sum(I < r) * 999999, c + sum(J < c) * 999999)
                    for r, c in galaxies]

total = sum([abs(g2[0] - g1[0]) + abs(g2[1] - g1[1])
             for g1, g2 in combinations(cosmic_expansion, 2)])

print("Part 2: " + str(total))
