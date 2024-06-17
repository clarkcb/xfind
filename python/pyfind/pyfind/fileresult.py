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
from io import StringIO
from pathlib import Path

from .filetypes import FileType


class FileResult:
    """encapsulates a file to evaluate as a find match and return if matching"""
    CONTAINER_SEPARATOR = '!'

    __slots__ = ['containers', 'path', 'file_type', 'file_size', 'last_mod']

    def __init__(self,
                 containers: list[Path] = None,
                 path: Path = None,
                 file_type: FileType = FileType.UNKNOWN,
                 file_size: int = 0,
                 last_mod: float = 0.0):
        self.containers = containers if containers else []
        self.path = path
        self.file_type = file_type
        self.file_size = file_size
        self.last_mod = last_mod

    @property
    def relative_path(self) -> Path:
        """Get relative path of FileResult (does not include any containers)"""
        return self.path

    def __str__(self):
        sio = StringIO()
        if self.containers:
            sio.write(self.CONTAINER_SEPARATOR.join([str(c) for c in self.containers]))
            sio.write(self.CONTAINER_SEPARATOR)
        sio.write(str(self.relative_path))
        return sio.getvalue()

    def __lt__(self, other):
        if self.path.parent == other.path.parent:
            return self.path.name < other.path.name
        return self.path.parent < other.path.parent

    def __eq__(self, other):
        return self.path == other.path
