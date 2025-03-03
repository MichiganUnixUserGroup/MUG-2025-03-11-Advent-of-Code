from collections import Counter


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
