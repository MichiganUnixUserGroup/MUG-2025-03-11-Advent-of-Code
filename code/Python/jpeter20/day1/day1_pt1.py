from sys import stdin

# Read all lines into arrays x and y

x = []
y = []
for line in stdin:
    p = line.split()
    x.append(int(p[0]))
    y.append(int(p[1]))

# Sort each array separately

x.sort()
y.sort()

# Accumulate differences between the two arrays, and report

diff = 0
for a, b in zip(x, y):
    print(f"[{a}, {b}]")
    diff += abs(a-b)

print("total difference is ", diff)
