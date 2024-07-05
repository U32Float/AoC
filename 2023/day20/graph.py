import networkx as nx
import matplotlib.pyplot as plt
import numpy as np

lines = [line.strip() for line in open("in.txt", "r")]

modules = {}

for line in lines:
    n, out = line.split(" -> ")
    out = out.split(", ")
    match n[0]:
        case "%":
            modules[n[1:]] = ("blue", out)
        case "&":
            modules[n[1:]] = ("red", out)
        case default:
            modules[n] = ("yellow", out)

graph = nx.DiGraph()
for name, (ty, out) in modules.items():
    for o in out:
        graph.add_edge(name, o)

modules["rx"] = ("green", [])

color_map = [modules[node][0] for node in graph.nodes()]


nx.draw_networkx(graph, arrows=True, node_color=color_map, with_labels=True)
plt.show()
