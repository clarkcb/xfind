# -*- coding: utf-8 -*-
"""
###############################################################################
#
# option.py
#
# class Option: abstract class that encapsulates a (command-line) option
#
###############################################################################
"""


class Option:
    """an abstract class to encapsulate a specific command line option"""
    __slots__ = ['short_arg', 'long_arg', 'desc', 'arg_type']
