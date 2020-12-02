import bisect
import logging
from itertools import permutations
from typing import List

from aoc2020.common.input import load_input

logger = logging.getLogger(__name__)

DAY = 1


def parse_input(data: str) -> List[int]:
    return [int(x) for x in data.splitlines()]


def solve_part_1() -> str:
    entries = sorted(parse_input(load_input(DAY)))

    options = permutations(entries, 2)
    for a, b in options:
        if a + b == 2020:
            return str(a * b)

    raise ValueError("No solution found")


def solve_part_2() -> str:
    entries = sorted(parse_input(load_input(DAY)))

    options = permutations(entries, 3)
    for a, b, c in options:
        if a + b + c == 2020:
            return str(a * b * c)

    raise ValueError("No solution found")
