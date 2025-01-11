#!/usr/bin/env python
import pandas as pd

# Read in the two lists.  We'll store them as two columns in the same DataFrame, `df`.
# My input file is not included.  You can use your own.
df = pd.read_csv("day1.input", sep=r"\s+", header=None, names=["left", "right"])

# Part 1

# Each of the two lists must be sorted.
df.left = df.left.sort_values(ignore_index=True)
df.right = df.right.sort_values(ignore_index=True)

# Create a new colum that is the distance, for each row of the lists, between the left and the right.
df["distance"] = (df.left - df.right).abs()

# The answer for part 1 is the sum of all the distances.
print(f"Day 1, part 1: the distance between the two lists is {df.distance.sum()}.")

# Part 2

# Build a dictionary where the keys are from the left list, and the values are the number of times
# those values appeared in the right list. 
right_counts = df.right.value_counts()

# Create a new column that is the similarity score, per value in the left column. `.get(x, 0)` looks
# up the count, and returns `0` if it's not found (that key never appeared in the right list).
df["similarity"] = df.left * df.left.map(lambda x: right_counts.get(x, 0))

# The answer for part 2 is the sum of all the similarities.
print(f"Day 1, part 2: the similarity score for the two lists is {df.similarity.sum()}.")
