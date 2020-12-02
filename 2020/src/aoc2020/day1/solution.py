import bisect
import logging
from typing import List

from aoc2020.common.input import load_input

logger = logging.getLogger(__name__)

DAY = 1


def parse_input(str) -> List[int]:
    return [int(x) for x in str.splitlines()]


def solve_part_1() -> str:
    entries = sorted(parse_input(load_input(DAY)))

    # NOTE: This could break if there is one entry of 1010
    # (half of 2020). I'm going to ignore that case as
    # it is not in my input.
    assert not any(x == 1010 for x in entries)

    for i, entry in enumerate(entries):
        target = 2020 - entry

        # a binary search against each entry gives us
        # O(n log n) instead of O(n^2)
        closest = entries[bisect.bisect_left(entries, target)]

        if closest == target:
            return str(entry * closest)

    raise ValueError("No solution found")


def solve_part_2() -> str:
    inpt = load_input(DAY)  # noqa: F841
    raise NotImplementedError
