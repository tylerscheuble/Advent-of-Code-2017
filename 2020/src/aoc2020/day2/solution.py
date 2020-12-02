import logging
import re
from dataclasses import dataclass
from typing import List, NewType, Optional

from aoc2020.common.input import load_input

logger = logging.getLogger(__name__)

DAY = 2

Password = NewType("Password", str)

password_regex = re.compile(r"^(\d+)-(\d+) (\w): (\w+)$")


def parse_password(data: str) -> Optional[Password]:
    match = password_regex.match(data)
    assert match

    policy_min = int(match[1])
    policy_max = int(match[2])
    policy_char = match[3]
    passwd = match[4]

    count = sum(1 for c in passwd if c == policy_char)

    if count < policy_min or count > policy_max:
        return None

    return Password(passwd)


def parse_input(data: str) -> List[Password]:
    result = []
    for line in data.splitlines():
        pwd = parse_password(line)
        if pwd is not None:
            result.append(pwd)

    return result


def solve_part_1() -> str:
    passwords = parse_input(load_input(DAY))
    return str(len(passwords))


def solve_part_2() -> str:
    inpt = load_input(DAY)  # noqa: F841
    raise NotImplementedError
