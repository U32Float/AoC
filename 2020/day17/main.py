import numpy as np
import sys
from scipy.ndimage import generic_filter

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

pocket = np.array([[c == "#" for c in line]for line in lines])

# ----------------------------------------------------------------------
# * Part 1


def kernel(X):
    center = X[len(X)//2]
    adj = np.sum(X) - center
    if center:
        return 2 <= adj <= 3
    else:
        return adj == 3


def solve(pocket, dim):
    for _ in range(dim - 2):
        pocket = np.expand_dims(pocket, 0)
    for _ in range(6):
        pocket = np.pad(pocket, 1, constant_values=False)
        pocket = generic_filter(pocket, kernel, size=3)

    return np.sum(pocket)


print("Part 1:", solve(pocket, 3))

# ----------------------------------------------------------------------
# * Part 2

print("Part 2:", solve(pocket, 4))
