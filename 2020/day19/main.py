from copy import deepcopy
from itertools import groupby
import sys

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

rules = {line.split(":")[0]: [list(filter(lambda x: x != "", l.split(" "))) for l in line.split(
    ":")[1].split("|")] for line in next(groupby(lines, lambda l: l == ""))[1]}

msgs = lines[lines.index("") + 1:]

# ----------------------------------------------------------------------
# * Part 1


def match(msg, r):
    if msg == "":
        return True, ""
    if len(rules[r]) == 1 and len(rules[r][0]) == 1 and len(rules[r][0][0]) == 3:
        return (msg[0] == rules[r][0][0][1], msg[1:])
    for rule in rules[r]:
        tmp = deepcopy(msg)
        valid = True
        for j in rule:
            m, tmp = match(tmp, j)
            if not m:
                valid = False
                break
        if not valid:
            continue
        return True,  tmp
    return False, []


print("Part 1:", len([m for m in msgs if match(m, "0") == (True, "")]))


# ----------------------------------------------------------------------
# * Part 2

rules["8"] = [["42", "8"], ["42"]]
rules["11"] = [["42", "11", "31"], ["42", "31"]]
a = len([m for m in msgs if match(m, "0") == (True, "")])
rules["8"] = [["42"], ["42", "8"]]
rules["11"] = [["42", "31"], ["42", "11", "31"]]
b = len([m for m in msgs if match(m, "0") == (True, "")])
print("Part 1:", b - a)
