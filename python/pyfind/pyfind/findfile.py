# -*- coding: utf-8 -*-
###############################################################################
#
# findfile.py
#
# class FindFile: encapsulates a file to evaluate as find match
#
###############################################################################
import os
from io import StringIO
from typing import List

from .filetypes import FileType


class FindFile(object):
    """encapsulates a file to evaluate as a find match"""
    CONTAINER_SEPARATOR = '!'

    __slots__ = ['containers', 'path', 'filename', 'filetype', 'stat']

    def __init__(self, containers: List[str] = None, path: str = '', filename: str = '',
                 filetype: FileType = FileType.UNKNOWN, stat=None):
        self.containers = containers if containers else []
        self.path = path
        self.filename = filename
        self.filetype = filetype
        self.stat = stat

    @property
    def relativepath(self):
        return os.path.join(self.path, self.filename)

    def __str__(self):
        sio = StringIO()
        if self.containers:
            sio.write(self.CONTAINER_SEPARATOR.join(self.containers))
            sio.write(self.CONTAINER_SEPARATOR)
        sio.write(self.relativepath)
        return sio.getvalue()

    def __lt__(self, other):
        if self.path == other.path:
            return self.filename < other.filename
        return self.path < other.path
    
    def __eq__(self, other):
        return self.path < other.path and self.filename < other.filename
