""" Generates a lookup table that checks whether a given set of corner
    values is equivalent to a topological disk.

    [Gerstner et al, 2000], as described in [Ju et al, 2002]
"""

def safe(index):
    f = [(index & (1 << i)) != 0 for i in range(8)]
    edges = [(0,1), (0,2), (2,3), (1,3),
             (4,5), (4,6), (6,7), (5,7),
             (0,4), (2,6), (1,5), (3,7)]

    def merge(a, b):
        merged = [(e[0] if e[0] != a else b,
                   e[1] if e[1] != a else b) for e in edges]
        return [e for e in merged if e[0] != e[1]]

    while True:
        for e in edges:
            if f[e[0]] == f[e[1]]:
                edges = merge(e[0], e[1])
                break
        else:
            break
    s = set(map(lambda t: tuple(sorted(t)),edges))
    return len(s) <= 1

out = ""
for i,s in enumerate([safe(i) for i in range(256)]):
    if out == "": out += "{"
    else: out += ","

    if i and i % 32 == 0:
        out += '\n '

    if s: out += "1"
    else: out += "0"
out += "}"
