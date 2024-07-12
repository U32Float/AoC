import networkx as nx
import math

lines = [line.strip() for line in open("in.txt", 'r')]
connections = [(line.split(': ')[0], line.split(': ')[1].split()) for line in lines]

G = nx.Graph()
for a, bs in connections:
    for b in bs:
        G.add_edge(a, b)

G.remove_edges_from(nx.minimum_edge_cut(G))

print(math.prod([len(c) for c in nx.connected_components(G)]))