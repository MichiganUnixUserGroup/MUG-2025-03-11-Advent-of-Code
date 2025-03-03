import pytest

# Production code never imports *.  Test code always imports *.
from day1 import *


@pytest.fixture()
def sample_input() -> tuple[list[int], list[int]]:
    left = [3, 4, 2, 1, 3, 3,]
    right = [4, 3, 5, 3, 9, 3,]

    return left, right


def test_read_the_two_lists(sample_input):
    left1, right1 = sample_input
    with open("tests/sample_lists.txt") as f:
        left2, right2 = read_the_two_lists(f)
    assert left1 == left2
    assert right1 == right2


def test_distance_between_the_two_lists(sample_input):
    left, right = sample_input
    assert distance_between_the_two_lists(left, right) == 11


def test_similarity_score(sample_input):
    left, right = sample_input
    assert similarity_score(left, right) == 31
