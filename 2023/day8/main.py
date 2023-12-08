import sys
import re
import math

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

pat = re.compile(r"((:?[A-Z]|\d){3})")

nodes = [[n[0] for n in pat.findall(line)] for line in lines[2:]]
graph = {n: [l, r] for n, l, r in nodes}

# ----------------------------------------------------------------------
# * Part 1

curr = 'AAA'
steps = 0
i = 0
while True:
    if lines[0][i] == 'L':
        curr = graph[curr][0]
    else:
        curr = graph[curr][1]
    steps += 1
    i = (i + 1) % len(lines[0])
    if curr == 'ZZZ':
        break

print("Part 1: " + str(steps))

# ----------------------------------------------------------------------
# * Part 2

starting_nodes = [key for key in graph.keys() if key[2] == 'A']

S = []
for node in starting_nodes:
    V = set()
    curr = node
    s = 0
    i = 0
    while True:
        if (i, curr) in V:
            break
        V.add((i, curr))

        if lines[0][i] == 'L':
            curr = graph[curr][0]
        else:
            curr = graph[curr][1]

        s += 1
        i = (i + 1) % len(lines[0])

        if curr[2] == 'Z':
            S.append(s)
            break


lcm = math.lcm(*S)

print("Part 2: " + str(lcm))
