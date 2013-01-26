import math
total = totalSq = n = 0
allOfThem = []
while True:
    try:
        a = float(raw_input())
    except:
        break
    total += a
    totalSq += a * a
    n += 1
    allOfThem.append(a)

varianceFull = (totalSq - total * total / n) / n
variance = (totalSq - total * total / n) / (n - 1)
srted = sorted(allOfThem)
measurements = [
    ("Total samples", n),
    ("Average value", total / n),
    ("Std deviation", math.sqrt(varianceFull)),
    ("Sample stddev", math.sqrt(variance)),
    ("Median", srted[len(allOfThem) / 2]),
    ("Min", srted[0]),
    ("Max", srted[-1]),
    ("Overall", str(total / n) + " +/- " + "%2.1f%%" %
        (100 * math.sqrt(variance) * n / total))
]
for label, value in measurements:
    print "%*s:" % (15, label), value
