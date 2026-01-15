from typing import List, Tuple, Type

from enum import IntEnum, StrEnum


def choices(enum_type: Type[StrEnum]) -> List[Tuple[(str, str)]]:
    return [(data.value, data.name) for data in enum_type]


def int_choices(enum_type: Type[IntEnum]) -> List[Tuple[(int, str)]]:
    return [(data.value, data.name) for data in enum_type]


def string_choices(choices_list: List[str]) -> List[Tuple[str, str]]:
    return [(choice, choice) for choice in choices_list]

