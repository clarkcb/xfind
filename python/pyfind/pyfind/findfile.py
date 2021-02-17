# -*- coding: utf-8 -*-
###############################################################################
#
# findfile.py
#
# class FindFile: encapsulates a file to find
#
###############################################################################
import os
from io import StringIO
from typing import List

from .filetypes import FileType


class FindFile(object):
    """encapsulates a find file"""
    CONTAINER_SEPARATOR = '!'

    __slots__ = ['containers', 'path', 'filename', 'filetype']

    def __init__(self, containers: List[str] = None, path: str = '', filename: str = '',
                 filetype: FileType = FileType.UNKNOWN):
        self.containers = containers if containers else []
        self.path = path
        self.filename = filename
        self.filetype = filetype

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
