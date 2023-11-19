import aocpy as aoc
from aocpy import print_bools, print_mat
import re
from enum import Enum
from copy import deepcopy
from pprint import pprint
from functools import reduce, cmp_to_key
from itertools import groupby, combinations, permutations
from time import time
from queue import PriorityQueue
from math import prod, ceil, comb, perm
from collections import defaultdict
from dataclasses import dataclass
from bisect import insort
import numpy as np
import string
import sys
import networkx as nx
import matplotlib.pyplot as plt

file = sys.argv[1] if len(sys.argv) > 1 else "ex"
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

S = [
    np.array(list(map(lambda l: tuple(map(int, l.split(","))), list(g))))
    for b, g in groupby(lines, lambda l: l == "" or l[:3] == "---")
    if not b
]
N = len(S)


# ----------------------------------------------------------------------
# * Part 1


def manhattan3d(P):
    X = np.tile(P[:, 0], (P.shape[0], 1))
    DX = np.abs(X - X.T)
    Y = np.tile(P[:, 1], (P.shape[0], 1))
    DY = np.abs(Y - Y.T)
    Z = np.tile(P[:, 2], (P.shape[0], 1))
    DZ = np.abs(Z - Z.T)
    return DX + DY + DZ


def least_squares(X, Y):
    X = np.hstack([X, np.ones((X.shape[0], 1))])
    Y = np.hstack([Y, np.ones((Y.shape[0], 1))])
    A, _, _, _ = np.linalg.lstsq(X, Y, rcond=None)
    return A


def match(A, B):
    SA = manhattan3d(A)
    SB = manhattan3d(B)
    M = []
    for i in range(SA.shape[0]):
        for j in range(SB.shape[0]):
            sa = set(SA[i, :])
            sb = set(SB[j, :])
            if len(sa.intersection(sb)) >= 11:
                M.append(A[i, :].tolist() + B[j, :].tolist())
                if len(M) == 4:
                    M = np.matrix(M)
                    T = least_squares(M[:, :3], M[:, 3:])
                    return T
    return None


def transform(A, X):
    return np.dot(np.hstack([X, np.ones((X.shape[0], 1))]), A)[:, :3]


M = {}
G = defaultdict(lambda: [])
for i in range(N):
    for j in range(N):
        if i >= j:
            continue
        m = match(S[i], S[j])
        n = match(S[j], S[i])
        if m is not None:
            G[j].append(i)
            G[i].append(j)
            M[(i, j)] = m
            M[(j, i)] = n


# N = nx.Graph()
# for i, J in G.items():
#     for j in J:
#         N.add_edge(i, j)
# nx.draw_networkx(N)
# plt.show()


def find_transform(root):
    stack = [(root, np.identity(4))]
    V = set()
    T = {}
    while stack:
        i, A = stack.pop()
        if i in V:
            continue
        V.add(i)
        T[i] = A
        for j in G[i]:
            B = M[(j, i)]
            stack.append((j, B @ A))
    return (T, V)


V = set()
T = {}
while True:
    for i in range(N):
        if i in V:
            continue
        t, v = find_transform(i)
        V |= v
        T |= t
        break
    else:
        break


R = []
for i, s in enumerate(S):
    r = transform(T[i], s)
    R.append(r)

R = np.vstack(R)


P = []
for r in R:
    valid = True
    for j in range(len(P)):
        if np.linalg.norm(np.array(P[j]) - np.array(r)) < 1:
            valid = False
            break
    if valid:
        P.append(r)

print("Part 1: {}".format(len(P)))


# ----------------------------------------------------------------------
# * Part 2

def manhattan(p, q):
    return np.sum(np.abs(p - q))


A = []
for i in range(N):
    A.append(transform(T[i], np.array([[0, 0, 0]]))[0, :])


maxd = -np.inf
for i in range(N):
    for j in range(N):
        if i >= j:
            continue
        maxd = max(maxd, manhattan(A[i], A[j]))

print("Part 2: {}".format(round(maxd)))
