from functools import lru_cache
from importlib import resources


@lru_cache(maxsize=None)
def load_input(day: int) -> str:
    return resources.read_text(f"aoc2020.day{day}", "input.txt")
