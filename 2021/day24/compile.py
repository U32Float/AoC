lines = list(map(lambda l: l.rstrip(), open("in.txt").readlines()))

# -----------------------------------------------------------------------------
# Unoptimized python:

MONAD = ["x = 0", "y = 0", "z = 0", "w = 0"]
i = 0
for line in lines:
    W = line.split()
    match W[0]:
        case "inp":
            MONAD.append("{} = input[{}]".format(W[1], i))
            i += 1
        case "add":
            MONAD.append("{} += {}".format(W[1], W[2]))
        case "mod":
            MONAD.append("{} %= {}".format(W[1], W[2]))
        case "div":
            MONAD.append("{} //= {}".format(W[1], W[2]))
        case "mul":
            MONAD.append("{} *= {}".format(W[1], W[2]))
        case "eql":
            MONAD.append("{} = int({} == {})".format(W[1], W[1], W[2]))

file = open("monad.py", "w")
file.write("def monad(input):\n  " + "\n  ".join(MONAD) + "\n  return z")
file.close()

# -----------------------------------------------------------------------------
# Optimized python:

"""
mul x 0
add x z
mod x 26
div z 1
add x 11
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 14
mul y x
add z y

if A <= 9 :
  w = input[0]
  x = z
  x %= 26
  z //= C
  x += A
  x = int(x != w)
  y = 25
  y *= x
  y += 1
  z *= y
  y = w
  y += B
  y *= x
  z += y
else:
  w = input[0]
  x = 1
  z //= C
  y = 26
  z *= y
  y = w
  y += B
  z += y
"""

MONAD = ["z = 0", "S = []"]
for i in range(14):
    a = int(lines[i * 18 + 5].split()[2])
    b = int(lines[i * 18 + 15].split()[2])

    # if a > 9 or a < -25:
    #     MONAD.extend(
    #         ["w = input[{}]".format(i),
    #          "z *= {}".format(26),
    #          "z += w + {}".format(b)])
    # else:
    #     MONAD.extend(
    #         ["w = input[{}]".format(i),
    #          "x = z",
    #          "x %= 26",
    #          "z //= 26",
    #          "x += {}".format(a),
    #          "if x != w:",  # x = 1
    #          "  z *= 26",
    #          "  y = w",
    #          "  y += {}".format(b),
    #          "  z += y",])
    if a > 9 or a < -25:
        MONAD.extend(
            ["w = input[{}]".format(i),
             "S.append(w + {})".format(b)])
    else:
        MONAD.extend(
            ["w = input[{}]".format(i),
             "x = S.pop()",
             "x += {}".format(a),
             "if x != w:",  # invariant: S[-1] + B[0] + A[1] == N[i]
             "  S.append(w + {})".format(b)])

MONAD.append("print(S)")
MONAD.append("z = sum(26**i * x for i, x in enumerate(S[::-1]))")

file = open("monad_opt.py", "w")
file.write("def monad(input):\n  " + "\n  ".join(MONAD) + "\n  return z")
file.close()

# -----------------------------------------------------------------------------
# Asm:

"""
w  -> x0
x  -> x1
y  -> x2
z  -> x3
26 -> x4
ptr -> x6
tmp -> x10
"""

MONAD = ["mov x3, 0", "mov x4, 26", "mov x6, {1}"]
for i in range(14):
    a = int(lines[i * 18 + 5].split()[2])
    b = int(lines[i * 18 + 15].split()[2])

    if a > 9 or a < -25:
        MONAD.extend(
            ["ldr x0, [x6, #{}]".format(i * 8),
             "mul x3, x3, x4",
             "mov x2, x0",
             "add x2, x2, #{}".format(
                 b) if b >= 0 else "sub x2, x2, #{}".format(-b),
             "add x3, x3, x2"])
    else:
        MONAD.extend(
            ["ldr x0, [x6, #{}]".format(i * 8),
             "mov x1, x3",
             "udiv x10, x1, x4",
             "msub x1, x10, x4, x1",
             "udiv x3, x3, x4",
             "add x1, x1, #{}".format(
                 a) if a >= 0 else "sub x1, x1, #{}".format(-a),
             "cmp x1, x0",
             "beq 1f",
             "mul x3, x3, x4",
             "mov x2, x0",
             "add x2, x2, #{}".format(
                 b) if b >= 0 else "sub x2, x2, #{}".format(-b),
             "add x3, x3, x2",
             "b 1f",
             "1:"])
    # if a > 9 or a < -25:
    #     MONAD.extend(
    #         ["ldr x0, [x6, #{}]".format(i * 8),
    #          "add x0, x0, #{}".format(
    #             b) if b >= 0 else "sub x2, x2, #{}".format(-b),
    #          "stp x0, x4, [sp, #-0x10]"])
    # else:
    #     MONAD.extend(
    #         ["ldr x0, [x6, #{}]".format(i * 8),
    #          "ldp x1, x4, [sp, #0x10]",
    #          "cmp x1, x0",
    #          "beq 1f",
    #          "mov x3, #1",
    #          "b 2f",
    #          "1:"])

MONAD.extend(["b 2f",
              "2:",
              "mov {0}, x3"])

file = open("monad.asm", "w")
file.write("\n".join(MONAD))
file.close()
