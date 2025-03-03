import sys

from collections import Counter
from typing import TextIO


def read_the_two_lists(input: TextIO) -> tuple[list[int], list[int]]:
    left: list[int] = []
    right: list[int] = []

    for line in input:
        left_str, right_str = line.split()
        left.append(int(left_str))
        right.append(int(right_str))

    return left, right


def distance_between_the_two_lists(left: list[int], right: list[int]) -> int:
    distance = 0
    # I'm careful not to sort left or right in-place.  Who knows what you want to do with them.
    # Yes, this for loop could have been a call to functools.reduce instead.  This is clearer.
    for x, y in zip(sorted(left), sorted(right)):
        distance += abs(x - y)
    return distance


def similarity_score(left: list[int], right: list[int]) -> int:
    right_counts = Counter(right)
    similarity = 0
    # Again, this for loop could have been a call to functools.reduce instead.  This is clearer.
    for x in left:
        similarity += x * right_counts[x]
    return similarity


if __name__ == "__main__":
    left, right = read_the_two_lists(sys.stdin)
    
    print(f"Day 1, part 1: the distance between the two lists is {distance_between_the_two_lists(left, right)}.")
    print(f"Day 1, part 2: the similarity score for the two lists is {similarity_score(left, right)}.")
