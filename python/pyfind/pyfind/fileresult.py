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
import os.path
from io import StringIO
from pathlib import Path

from .color import Color
from .filetypes import FileType
from .findsettings import FindSettings


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


class FileResultFormatter(object):
    """provides formatting of FileResult instances"""

    def __init__(self, settings: FindSettings):
        self.settings = settings

    @staticmethod
    def colorize(s: str, match_start_index: int, match_end_index: int) -> str:
        return s[0:match_start_index] + Color.GREEN + \
            s[match_start_index:match_end_index] + \
            Color.RESET + s[match_end_index:]

    def format_path(self, path: Path) -> str:
        """format a path"""
        file_name = path.name
        for p in self.settings.in_file_patterns:
            match = p.search(file_name)
            if match:
                file_name = FileResultFormatter.colorize(
                    file_name, match.start(), match.end())
                break
        return os.path.join(str(path.parent), file_name)

    def format_file_result(self, result: FileResult) -> str:
        """format a FileResult instance"""
        return self.format_path(result.path)
