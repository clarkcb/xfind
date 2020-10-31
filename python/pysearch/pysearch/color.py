# -*- coding: utf-8 -*-
###############################################################################
#
# color.py
#
# enum class Color
#
###############################################################################
from enum import Enum


class Color(object):
    """Color enum"""
    GREY = '\033[30m'
    RED = '\033[31m'
    GREEN = '\033[32m'
    YELLOW = '\033[33m'
    BLUE = '\033[34m'
    RESET = '\033[0m'