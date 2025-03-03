import sys

from pathlib import Path
from typing import Optional
from typing_extensions import Annotated

import typer

from list_tools import read_the_two_lists, distance_between_the_two_lists, similarity_score


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
