from typing import List, Tuple, Type
from enum import StrEnum


def choices(enum_type: Type[StrEnum]) -> List[Tuple[(str, str)]]:
    return [(data.value, data.name) for data in enum_type]
