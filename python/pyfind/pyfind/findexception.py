# -*- coding: utf-8 -*-
"""
###############################################################################
#
# findexception.py
#
# class FindException: custom exception
#
###############################################################################
"""
class FindException(Exception):
    """a class to represent an exception to raise for find issues"""
    def __init__(self, *args):
        Exception.__init__(self, *args)
