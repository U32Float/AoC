import sympy as sym

lines = [line.strip() for line in open("in.txt", 'r')]
hailstones = [[tuple(map(int, s.split(", "))) for s in line.split(" @ ")] for line in lines]


x,y,z,dx,dy,dz = sym.symbols('x,y,z,dx,dy,dz')

T = sym.symbols(",".join(['t' + str(i) for i in range(len(hailstones))]))

equations = []
for t, ((hx, hy, hz), (vx, vy, vz)) in zip(T[:3], hailstones[:3]):
    equations += [
        sym.Eq(x + dx*t, hx + vx*t),
        sym.Eq(y + dy*t, hy + vy*t),
        sym.Eq(z + dz*t, hz + vz*t),
        sym.GreaterThan(t, 0)
    ]


symbols = [x, y, z, dx, dy, dz] + list(T)


p = sym.nonlinsolve(equations, symbols)

print("Part 2: " + str(sum(int(s) for s in str(p)[2:].split(",")[:3])))


