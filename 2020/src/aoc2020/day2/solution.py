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


def parse_password_2(data: str) -> Optional[Password]:
    match = password_regex.match(data)
    assert match

    policy_i = int(match[1]) - 1
    policy_j = int(match[2]) - 1
    policy_char = match[3]
    passwd = match[4]

    if (passwd[policy_i] == policy_char) ^ (passwd[policy_j] == policy_char):
        return Password(passwd)

    return None


def parse_input(data: str, new_criteria: bool = False) -> List[Password]:
    result = []
    for line in data.splitlines():
        if new_criteria:
            pwd = parse_password_2(line)
        else:
            pwd = parse_password(line)

        if pwd is not None:
            result.append(pwd)

    return result


def solve_part_1() -> str:
    passwords = parse_input(load_input(DAY))
    return str(len(passwords))


def solve_part_2() -> str:
    passwords = parse_input(load_input(DAY), True)
    return str(len(passwords))
