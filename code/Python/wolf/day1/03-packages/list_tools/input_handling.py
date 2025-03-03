from typing import TextIO

def read_the_two_lists(input: TextIO) -> tuple[list[int], list[int]]:
    left: list[int] = []
    right: list[int] = []

    for line in input:
        left_str, right_str = line.split()
        left.append(int(left_str))
        right.append(int(right_str))

    return left, right
