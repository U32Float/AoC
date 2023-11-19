import re
from enum import Enum
from copy import deepcopy
from pprint import pprint
from functools import reduce, cmp_to_key
from itertools import groupby, combinations, permutations
from time import time
from queue import PriorityQueue
from math import prod, ceil, comb, perm
from collections import defaultdict, Counter
from dataclasses import dataclass
from bisect import insort
from operator import add, mul
from functools import lru_cache
import numpy as np
import string
import sys
from sys import exit
import bpy
import bmesh

argv = sys.argv[sys.argv.index("--") + 1:]
file = argv[0]
lines = list(map(lambda l: l.rstrip(), open(file + ".txt").readlines()))

# ----------------------------------------------------------------------
# * Parsing(/setup)

pat = re.compile(r"x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)")
instructions = [(line[:2] == "on", list(map(int, pat.search(line).groups())))
                for line in lines]

# ----------------------------------------------------------------------
# * Part 1


def trim(r):
    return range(max(r.start, 0), min(r.stop, 101)) or None


cubes = np.zeros((101, 101, 101), dtype=bool)

for on, cuboid in instructions:
    x1, x2, y1, y2, z1, z2 = map(lambda x: x + 50, cuboid)
    X = trim(range(x1, x2+1))
    Y = trim(range(y1, y2+1))
    Z = trim(range(z1, z2+1))
    if X != None and Y != None and Z != None:
        cubes[X.start:X.stop, Y.start:Y.stop, Z.start:Z.stop] = on

print("Part 1: {}".format(np.sum(cubes)))

# ----------------------------------------------------------------------
# * Part 2


def cuboid(id, x1, x2, y1, y2, z1, z2):
    vertices = [(x1, y1, z1), (x1, y1, z2), (x1, y2, z1), (x1, y2, z2),
                (x2, y1, z1), (x2, y1, z2), (x2, y2, z1), (x2, y2, z2)]
    edges = [(0, 1), (0, 2), (0, 4), (1, 3), (1, 5), (2, 3), (2, 6), (3, 7),
             (4, 5), (4, 6), (5, 7), (6, 7)]
    faces = [(0, 1, 3, 2), (0, 1, 5, 4), (0, 2, 6, 4), (1, 3, 7, 5),
             (2, 3, 7, 6), (4, 5, 7, 6)]

    mesh = bpy.data.meshes.new(id)
    mesh.from_pydata(vertices, edges, faces)
    mesh.update()
    obj = bpy.data.objects.new(id, mesh)
    bpy.context.scene.collection.objects.link(obj)

    bpy.context.view_layer.objects.active = obj
    bpy.ops.object.mode_set(mode='EDIT')
    bpy.ops.mesh.select_all(action='SELECT')
    bpy.ops.mesh.normals_make_consistent(inside=False)
    bpy.ops.object.mode_set(mode='OBJECT')

    return obj


minx, miny, minz = (np.inf, np.inf, np.inf)
for _, [x1, x2, y1, y2, z1, z2] in instructions:
    minx = min(minx, x1)
    miny = min(miny, y1)
    minz = min(minz, z1)


S = 20
reactor = None
for on, [x1, x2, y1, y2, z1, z2] in instructions:
    x1 = (x1 - minx) * S
    x2 = (x2 - minx + 1) * S
    y1 = (y1 - miny) * S
    y2 = (y2 - miny + 1) * S
    z1 = (z1 - minz) * S
    z2 = (z2 - minz + 1) * S

    if reactor == None:
        reactor = cuboid("reactor", x1, x2, y1, y2, z1, z2)
        continue

    c = cuboid("cuboid", x1, x2, y1, y2, z1, z2)

    bool = reactor.modifiers.new(type="BOOLEAN", name="bool")
    bool.object = c
    bool.operation = "UNION" if on else "DIFFERENCE"

    bpy.context.view_layer.objects.active = reactor
    bpy.ops.object.modifier_apply(modifier="bool")
    bpy.data.objects.remove(bpy.data.objects['cuboid'], do_unlink=True)

bpy.context.view_layer.objects.active = reactor
bm = bmesh.new()
bm.from_mesh(bpy.context.object.data)
bm.normal_update()
volume = round(float(bm.calc_volume()) / (S**3)) + 1

print("Part 2: {}".format(volume))
