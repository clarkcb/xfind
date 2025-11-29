# -*- coding: utf-8 -*-
"""
###############################################################################
#
# color.py
#
# Color class - generic colors enum, conversions to ConsoleColor
#
###############################################################################
"""
from enum import StrEnum

from .consolecolor import ConsoleColor


class Color(StrEnum):
    """Color enum"""
    BLACK = 'black'
    RED = 'red'
    GREEN = 'green'
    YELLOW = 'yellow'
    BLUE = 'blue'
    MAGENTA = 'magenta'
    CYAN = 'cyan'
    WHITE = 'white'

    @classmethod
    def _missing_(cls, value):
        value = str(value).strip().lower()
        for member in cls:
            if member == value:
                return member
        return cls.BLACK

    @classmethod
    def to_console_color(cls, value):
        color = cls._missing_(value)
        match color:
            case cls.BLACK:
                return ConsoleColor.BLACK
            case cls.RED:
                return ConsoleColor.RED
            case cls.GREEN:
                return ConsoleColor.GREEN
            case cls.YELLOW:
                return ConsoleColor.YELLOW
            case cls.BLUE:
                return ConsoleColor.BLUE
            case cls.MAGENTA:
                return ConsoleColor.MAGENTA
            case cls.CYAN:
                return ConsoleColor.CYAN
            case cls.WHITE:
                return ConsoleColor.WHITE

