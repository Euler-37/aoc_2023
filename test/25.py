g={}
with open("data/test25.txt") as f:
    for line in f.readlines():
        h,l=line.split(": ")
        if h not in g:
            g[h]=[]
        for s in l.split():
            g[h].append(s)
            if s not in g:
                g[s]=[]
            g[s].append(h)
g
