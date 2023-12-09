import sys
import numpy as np

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

history = np.mat([[int(x) for x in line.split()] for line in lines])

# ----------------------------------------------------------------------
# * Part 1

diffs = [history]
while not np.all(diffs[-1] == 0):
    diffs.append(np.diff(diffs[-1]))

total = sum(sum([d[:, -1]for d in diffs]))[0, 0]

print("Part 1: " + str(total))

# ----------------------------------------------------------------------
# * Part 2

acc = 0
[acc := d[:, 0] - acc for d in diffs]
total = abs(sum(acc))[0, 0]

print("Part 2: " + str(total))
