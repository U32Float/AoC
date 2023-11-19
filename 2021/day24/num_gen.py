from random import randint
import monad as m
import monad_opt as opt
import importlib as imp
imp.reload(m)
imp.reload(opt)

N = []
for _ in range(10_000):
    num = [randint(1, 9) for _ in range(14)]
    N.append("{} {}".format("".join(map(str, num)), m.monad(num)))

    assert (m.monad(num) == opt.monad(num))

file = open("nums.txt", "w")
file.write("\n".join(N))
file.close()
