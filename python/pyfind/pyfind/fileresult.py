# -*- coding: utf-8 -*-
"""
###############################################################################
#
# fileresult.py
#
# class FileResult: encapsulates a file to evaluate as find match and return
#                   if matching
#
###############################################################################
"""
import os
from io import StringIO
from pathlib import Path
from typing import List

from .filetypes import FileType


class FileResult:
    """encapsulates a file to evaluate as a find match and return if matching"""
    CONTAINER_SEPARATOR = '!'

    __slots__ = ['containers', 'path', 'file_type', 'stat']

    def __init__(self,
                 containers: List[str] = None,
                 path: Path = None,
                 file_type: FileType = FileType.UNKNOWN,
                 stat: os.stat_result = None):
        self.containers = containers if containers else []
        self.path = path
        self.file_type = file_type
        self.stat = stat

    @property
    def relative_path(self) -> str:
        """Get relative path of FileResult (does not include any containers)"""
        return str(self.path)

    def __str__(self):
        sio = StringIO()
        if self.containers:
            sio.write(self.CONTAINER_SEPARATOR.join(self.containers))
            sio.write(self.CONTAINER_SEPARATOR)
        sio.write(self.relative_path)
        return sio.getvalue()

    def __lt__(self, other):
        if self.path.parent == other.path.parent:
            return self.path.name < other.path.name
        return self.path.parent < other.path.parent

    def __eq__(self, other):
        return self.path == other.path
