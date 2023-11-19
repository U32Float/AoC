def monad(input):
  z = 0
  S = []
  w = input[0]
  S.append(w + 14)
  w = input[1]
  S.append(w + 8)
  w = input[2]
  S.append(w + 4)
  w = input[3]
  S.append(w + 10)
  w = input[4]
  x = S.pop()
  x += -3
  if x != w:
    S.append(w + 14)
  w = input[5]
  x = S.pop()
  x += -4
  if x != w:
    S.append(w + 10)
  w = input[6]
  S.append(w + 4)
  w = input[7]
  x = S.pop()
  x += -8
  if x != w:
    S.append(w + 14)
  w = input[8]
  x = S.pop()
  x += -3
  if x != w:
    S.append(w + 1)
  w = input[9]
  x = S.pop()
  x += -12
  if x != w:
    S.append(w + 6)
  w = input[10]
  S.append(w + 0)
  w = input[11]
  x = S.pop()
  x += -6
  if x != w:
    S.append(w + 9)
  w = input[12]
  S.append(w + 13)
  w = input[13]
  x = S.pop()
  x += -12
  if x != w:
    S.append(w + 12)
  print(S)
  z = sum(26**i * x for i, x in enumerate(S[::-1]))
  return z