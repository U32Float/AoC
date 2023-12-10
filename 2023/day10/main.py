import sys
import numpy as np
from scipy.ndimage import generic_filter

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

grid = np.mat([[c for c in line]for line in lines])
start = tuple(np.argwhere(grid == 'S')[0])

DIRS = {
    'S': [(0, 1), (0, -1), (-1, 0), (1, 0)],
    'L': [(-1, 0), (0, 1)],
    'J': [(-1, 0), (0, -1)],
    '7': [(1, 0), (0, -1)],
    'F': [(1, 0), (0, 1)],
    '|': [(-1, 0), (1, 0)],
    '-': [(0, -1), (0, 1)],
    '.': [],
}

# ----------------------------------------------------------------------
# * Part 1


path = []
curr = start
while True:
    if curr == start and path:
        break
    for dir in DIRS[grid[curr[0], curr[1]]]:
        p = (curr[0] + dir[0], curr[1] + dir[1])
        if path and p == path[-1]:
            continue
        if p[0] >= 0 and p[0] < grid.shape[0] and p[1] >= 0 and p[1] < grid.shape[1]:
            if (-dir[0], -dir[1]) not in DIRS[grid[p[0], p[1]]]:
                continue
            path.append(curr)
            curr = p
            break


print("Part 1: " + str(len(path) // 2))

# ----------------------------------------------------------------------
# * Part 2


def rot_90(v):
    match v:
        case (0, 1):
            return (1, 0)
        case (0, -1):
            return (-1, 0)
        case (1, 0):
            return (0, -1)
        case (-1, 0):
            return (0, 1)


def fill(K):
    if K[4] == 0 and (K[1] == 1 or K[3] == 1 or K[5] == 1 or K[7] == 1):
        return 1
    return K[4]


M = np.full(grid.shape, 0)
M[start[0], start[1]] = 5

total = 0
for i in range(1, len(path)):
    prev = path[i - 1]
    curr = path[i]
    delta = (curr[0] - prev[0], curr[1] - prev[1])
    M[curr[0], curr[1]] = 5

    v = rot_90(delta)

    r = prev[0] - v[0]
    c = prev[1] - v[1]
    if r >= 0 and r < M.shape[0] and c >= 0 and c < M.shape[1]:
        if M[r, c] == 0:
            M[r, c] = 1

    r = curr[0] - v[0]
    c = curr[1] - v[1]
    if r >= 0 and r < M.shape[0] and c >= 0 and c < M.shape[1]:
        if M[r, c] == 0:
            M[r, c] = 1


for _ in range(10):
    M = generic_filter(M, fill, 3)

total = len(np.argwhere(M == 1))

print("Part 2: " + str(total))
