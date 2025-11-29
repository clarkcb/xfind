# -*- coding: utf-8 -*-
"""
###############################################################################
#
# consolecolor.py
#
# ConsoleColor class - console colors
#
###############################################################################
"""


class ConsoleColor:
    """ConsoleColor class"""
    _esc = '\033'
    _reset = 0
    _regular = 0
    _bold = 1
    _black = 30
    _red = 31
    _green = 32
    _yellow = 33
    _blue = 34
    _magenta = 35
    _cyan = 36
    _white = 37

    RESET = f'{_esc}[{_reset}m'
    BLACK = f'{_esc}[{_regular};{_black}m'
    RED = f'{_esc}[{_regular};{_red}m'
    GREEN = f'{_esc}[{_regular};{_green}m'
    YELLOW = f'\033[{_regular};{_yellow}m'
    BLUE = f'{_esc}[{_regular};{_blue}m'
    MAGENTA = f'{_esc}[{_regular};{_magenta}m'
    CYAN = f'{_esc}[{_regular};{_cyan}m'
    WHITE = f'{_esc}[{_regular};{_white}m'

    BOLD_BLACK = f'{_esc}[{_bold};{_black}m'
    BOLD_RED = f'{_esc}[{_bold};{_red}m'
    BOLD_GREEN = f'{_esc}[{_bold};{_green}m'
    BOLD_YELLOW = f'{_esc}[{_bold};{_yellow}m'
    BOLD_BLUE = f'{_esc}[{_bold};{_blue}m'
    BOLD_MAGENTA = f'{_esc}[{_bold};{_magenta}m'
    BOLD_CYAN = f'{_esc}[{_bold};{_cyan}m'
    BOLD_WHITE = f'{_esc}[{_bold};{_white}m'
