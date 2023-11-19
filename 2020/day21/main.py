from copy import deepcopy
from collections import Counter
import sys

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

I = [l.split("contains ")[0].split()[:-1] for l in lines]
A = [l.split("contains ")[1][:-1].split(", ") for l in lines]
ingredients = set(sum(I, []))
allergens = set(sum(A, []))

# ----------------------------------------------------------------------
# * Part 1

inert = deepcopy(ingredients)
for allergen in allergens:
    inert -= set.intersection(*[set(i)
                                for i, a in zip(I, A) if allergen in a])

amount = Counter(sum(I, []))
print("Part 1:", sum(amount[i] for i in inert))

# ----------------------------------------------------------------------
# * Part 2


candidates = {i: deepcopy(allergens) for i in ingredients - inert}

for allergen in allergens:
    for ingredient in ingredients - inert:
        if not all(ingredient in i for i, a in zip(I, A) if allergen in a):
            candidates[ingredient].remove(allergen)

res = []
V = set()
while any(len(c) > 1 for c in candidates.values()):
    for ingredient, allergens in candidates.items():
        if len(allergens) == 1 and str(allergens) not in V:
            res.append((ingredient, list(allergens)[0]))
            for other in candidates:
                if other != ingredient:
                    candidates[other] -= allergens
            V.add(str(allergens))

res.sort(key=lambda x: x[1])
print("Part 2:", ",".join(i for i, _ in res))
