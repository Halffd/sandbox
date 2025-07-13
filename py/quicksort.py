def qsort(xs):
    return [] if not xs else qsort([x for x in xs[1:] if x <= xs[0]]) + [xs[0]] + qsort([x for x in xs[1:] if x > xs[0]])
print(qsort([8,6,4,2,2,1]))
print(qsort([-1,11,-675, 95599, 443, 3333, *range(1,1500)]))

