from math import prod
from operator import add, mul
import sys

file = sys.argv[1] if len(sys.argv) > 1 else "ex1"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

expressions = [line.replace(" ", "") for line in lines]

# ----------------------------------------------------------------------
# * Part 1


def evaluate(expr):
    val = None
    op = None
    i = 0
    while i < len(expr):
        if expr[i] == "(":
            v, r = evaluate(expr[i+1:])
            if op is None:
                val = v
            else:
                val = op(val, v)
            expr = r
            i = 0
            continue
        elif expr[i] == ")":
            return val, expr[i+1:]
        elif expr[i] == "+":
            op = add
        elif expr[i] == "*":
            op = mul
        else:
            if op is None:
                val = int(expr[i])
            else:
                val = op(val, int(expr[i]))
        i += 1
    return val, []


t = 0
for expr in expressions:
    t += evaluate(expr)[0]

print("Part 1:", t)

# ----------------------------------------------------------------------
# * Part 2


def evaluate2(expr):
    stack = []
    add = False
    i = 0
    while i < len(expr):
        x = expr[i]
        v = None
        if x == "+":
            add = True
            i += 1
            continue
        elif x == "(":
            v, r = evaluate2(expr[i+1:])
            expr = r
            i = -1
        elif x == ")":
            return prod(stack), expr[i+1:]
        elif x == "*":
            i += 1
            continue
        else:
            v = int(x)
        if add:
            add = False
            stack[-1] += v
        else:
            stack.append(v)
        i += 1
    return prod(stack), []


t = 0
for expr in expressions:
    t += evaluate2(expr)[0]

print("Part 2:", t)
