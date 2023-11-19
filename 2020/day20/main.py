from itertools import groupby, combinations
from math import prod, sqrt
import numpy as np
import sys

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

ids = [int(line[5:9]) for line in lines if line.startswith("Tile")]
data = [np.array([[l == "#" for l in line]for line in list(g)[1:]])
        for f, g in groupby(lines, lambda l: l == "") if not f]
monster = np.array([[l == "#" for l in line[:20]]
                   for line in open("monster.txt").readlines()])
n = len(data)
size = int(sqrt(n))

rotated = [[[np.rot90(np.fliplr(data[i]) if f else data[i], k=r)
             for f in [True, False]] for r in range(4)] for i in range(n)]

adj = np.full((n, n), False, dtype=bool)
full = np.arange(10)
edges = [(([0], full), ([-1], full)), ((full, [0]), (full, [-1])), (([-1], full), ([0], full)),
         ((full, [-1]), (full, [0]))]
transforms = [(0, False), (1, False), (2, False), (3, False),
              (0, True), (1, True), (2, True), (3, True)]
for i, j in combinations(range(n), 2):
    for r1, f1 in transforms:
        a = data[i]
        b = rotated[j][r1][f1]
        for k, (e1, e2) in enumerate(edges):
            if (a[e1] == b[e2]).all():
                adj[i][j] = True
                adj[j][i] = True
                break


def corners_edges():
    corners = [i for i in range(n) if np.sum(adj[i]) == 2]
    edges = [i for i in range(n) if np.sum(adj[i]) == 3]
    return corners, edges

# ----------------------------------------------------------------------
# * Part 1

corners, edges, = corners_edges()
print("Part 1:", prod(ids[i] for i in corners))

# ----------------------------------------------------------------------
# * Part 2

def unfold(prev, pos):
    if pos[0] == size or pos[1] == size:
        return
    if img[pos] != -1:
        return

    for i in range(n):
        if placed[i]:
            continue
        if not adj[img[prev]][i]:
            continue
        for r1, f1 in transforms:
            a = rotated[img[prev]][rots[prev]][int(flips[prev])]
            b = rotated[i][r1][f1]

            dy, dx = ((pos[0] - prev[0]), (pos[1] - prev[1]))
            match (dy, dx):
                case (-1, 0):
                    if not (a[0] == b[-1]).all():
                        continue
                case (1, 0):
                    if not (a[-1] == b[0]).all():
                        continue
                case (0, -1):
                    if not (a[:, 0] == b[:, -1]).all():
                        continue
                case (0, 1):
                    if not (a[:, -1] == b[:, 0]).all():
                        continue
            img[pos] = i
            rots[pos] = r1
            flips[pos] = f1
            placed[i] = True
            unfold(pos, (pos[0] + 1, pos[1]))
            unfold(pos, (pos[0], pos[1] + 1))


for r, f in transforms:
    img = np.full((size, size), -1, dtype=int)
    img[0][0] = corners[0]
    rots = np.full((size, size), 0, dtype=int)
    rots[0][0] = r
    flips = np.full((size, size), False, dtype=bool)
    flips[0][0] = f
    placed = np.full(n, False, dtype=bool)
    placed[corners[0]] = True
    unfold((0, 0), (0, 1))
    unfold((0, 0), (1, 0))
    if (img != -1).all():
        break

full_img = []
for r in range(size):
    row = []
    for c in range(size):
        row.append(rotated[img[r][c]][rots[r][c]]
                   [int(flips[r][c])][1:-1, 1:-1])
    full_img.append(np.hstack(row))
full_img = np.vstack(full_img)

for r, f in transforms:
    img = np.rot90(full_img, k=r)
    if f:
        img = np.fliplr(img)
    m = 0
    for i in range(size*8 - 3):
        for j in range(size*8 - 20):
            if np.sum(img[i:i+3, j:j+20] & monster) == np.sum(monster):
                m += 1
    if m > 0:
        print("Part 2:", np.sum(img) - m * np.sum(monster))
        break
