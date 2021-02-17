# -*- coding: utf-8 -*-
###############################################################################
#
# findexception.py
#
# class FindException: custom exception
#
###############################################################################


class FindException(Exception):
    def __init__(self, *args):
        Exception.__init__(self, *args)
