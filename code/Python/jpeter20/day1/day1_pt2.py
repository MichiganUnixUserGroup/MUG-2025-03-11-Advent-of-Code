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

# Similarity: Find the number of times each number in the left list appears
# in the right list, and accumulate the number itself times the number of
# times it appears.

similarity = 0
for a in x:
    count = 0
    for b in y:
        if b > a:
            break  # save time by quitting when appropriate
        if a == b:
            count += 1
    if count > 0:
        print("found ", a, count, " times in the second list")
    similarity += a*count

print("similarity is ", similarity)
