# -*- coding: utf-8 -*-
"""
###############################################################################
#
# color.py
#
# Color class - console colors
#
###############################################################################
"""
class ConsoleColor:
    """ConsoleColor class"""
    _prefix = '\033['

    RESET = f'\033[0m'
    BLACK = f'\033[0;30m'
    RED = f'\033[0;31m'
    GREEN = f'\033[0;32m'
    YELLOW = f'\033[0;33m'
    BLUE = f'\033[0;34m'
    MAGENTA = f'\033[0;35m'
    CYAN = f'\033[0;36m'
    WHITE = f'\033[0;37m'

    BOLD_BLACK = f'\033[1;30m'
    BOLD_RED = f'\033[1;31m'
    BOLD_GREEN = f'\033[1;32m'
    BOLD_YELLOW = f'\033[1;33m'
    BOLD_BLUE = f'\033[1;34m'
    BOLD_MAGENTA = f'\033[1;35m'
    BOLD_CYAN = f'\033[1;36m'
    BOLD_WHITE = f'\033[1;37m'
