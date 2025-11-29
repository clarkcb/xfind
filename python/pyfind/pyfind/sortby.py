# -*- coding: utf-8 -*-
"""
###############################################################################
#
# sortby.py
#
# class SortBy: enum of sorting options
#
###############################################################################
"""
from enum import StrEnum


class SortBy(StrEnum):
    """SortBy enum"""
    FILEPATH = 'filepath'
    FILENAME = 'filename'
    FILETYPE = 'filetype'
    FILESIZE = 'filesize'
    LASTMOD = 'lastmod'

    @classmethod
    def _missing_(cls, value):
        value = str(value).strip().lower()
        for member in cls:
            if member == value or member == f'file{value}':
                return member
        return cls.FILEPATH
