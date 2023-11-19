from copy import deepcopy
import sys

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

labels = list(map(int, lines[0]))

# ----------------------------------------------------------------------
# * Part 1

next = deepcopy(labels)
for _ in range(100):
    for i in range(1, len(next)):
        v = (next[0] - i) % 10
        if v == 0:
            continue
        if v not in next[1:4]:
            j = next.index(v)
            next = next[4:j+1] + next[1:4] + next[j+1:] + [next[0]]
            break

i = next.index(1)
next = next[i+1:] + next[:i]
print("Part 1:", "".join(map(str, next)))

# ----------------------------------------------------------------------
# * Part 2

X = deepcopy(labels) + list(range(10, 1_000_001))
next = [0] * (1_000_001)
for i in range(len(X)-1):
    next[X[i]] = X[i+1]
next[X[-1]] = X[0]

cur = X[0]
for _ in range(10_000_000):
    x = next[cur]
    y = next[x]
    z = next[y]
    new = next[z]
    dest = cur - 1
    while True:
        if dest == 0:
            dest = 1_000_000
        if dest not in (x, y, z):
            break
        dest = dest - 1
    next[cur] = new
    next[z] = next[dest]
    next[dest] = x
    cur = new

print("Part 2:", next[1] * next[next[1]])
