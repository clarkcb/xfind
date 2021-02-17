# -*- coding: utf-8 -*-
###############################################################################
#
# findoption.py
#
# class FindOption: encapsulates a (command-line) find option
#
###############################################################################


class FindOption(object):
    """a class to encapsulate a specific command line option"""

    __slots__ = ['shortarg', 'longarg', 'desc', 'func']

    def __init__(self, shortarg: str, longarg: str, desc: str, func):
        self.shortarg = shortarg
        self.longarg = longarg
        self.desc = desc
        self.func = func

    @property
    def sortarg(self):
        if self.shortarg:
            return self.shortarg.lower() + 'a' + self.longarg.lower()
        return self.longarg.lower()
