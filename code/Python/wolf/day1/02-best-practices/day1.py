import sys

from collections import Counter
from pathlib import Path
from typing import Optional, TextIO
from typing_extensions import Annotated

import typer


__all__ = [
    "read_the_two_lists",
    "distance_between_the_two_lists",
    "similarity_score",
]


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


def main(
        # You provide options to just see one part or the other.  If you don't say anything, you'll get both.
        show_distance: Annotated[Optional[bool], typer.Option("--distance", "-d")] = False,
        show_similarity: Annotated[Optional[bool], typer.Option("--similarity", "-s")] = False,

        # There's an optional argument: the path to an input file.  If missing, I'll use stdin.
        input: Annotated[Optional[Path], typer.Argument()] = None
):
    if input is None:
        left, right = read_the_two_lists(sys.stdin)
    else:
        with open(input) as f:
            left, right = read_the_two_lists(f)

    # If you don't provide _any_ options, then show the answers to both parts of the problem.
    if not show_distance and not show_similarity:
        show_distance = True
        show_similarity = True
    
    if show_distance:
        print(f"Day 1, part 1: the distance between the two lists is {distance_between_the_two_lists(left, right)}.")
    if show_similarity:
        print(f"Day 1, part 2: the similarity score for the two lists is {similarity_score(left, right)}.")


if __name__ == "__main__":
    typer.run(main)
