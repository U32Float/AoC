import sys

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

cards = [[s.split() for s in line[7:].split('|')] for line in lines]

# ----------------------------------------------------------------------
# * Part 1


def value(n):
    if n == 0:
        return 0
    elif n == 1:
        return 1
    else:
        return 2*value(n-1)


total = 0
for card, nums in cards:
    matches = set(card).intersection(set(nums))
    total += value(len(matches))

print("Part 1: " + str(total))

# ----------------------------------------------------------------------
# * Part 2

N = len(cards)

stack = [1 for _ in range(N)]
for i, [card, nums] in enumerate(cards):
    matches = set(card).intersection(set(nums))
    n = len(matches)
    for j in range(1, n+1):
        if i + j < N:
            stack[i+j] += stack[i]

total = sum(stack)
print("Part 2: " + str(total))
