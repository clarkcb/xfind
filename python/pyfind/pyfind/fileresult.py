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
        if settings.colorize:
            if settings.in_dir_patterns:
                self.format_dir_path = self.__format_dir_path_with_color
            if settings.in_extensions or settings.in_file_patterns:
                self.format_file_name = self.__format_file_name_with_color

    @staticmethod
    def colorize(s: str, match_start_index: int, match_end_index: int) -> str:
        """colorize a string"""
        prefix = ''
        if match_start_index > 0:
            prefix = s[0:match_start_index]
        suffix = ''
        if match_end_index < len(s):
            suffix = s[match_end_index:]
        return prefix + \
            Color.GREEN + \
            s[match_start_index:match_end_index] + \
            Color.RESET + \
            suffix

    def __format_dir_path_with_color(self, dir_path: Path) -> str:
        """format a dir path, highlighting matches with color"""
        formatted_dir = str(dir_path)
        for p in self.settings.in_dir_patterns:
            match = p.search(formatted_dir)
            if match:
                formatted_dir = FileResultFormatter.colorize(
                    formatted_dir, match.start(), match.end())
                break
        return formatted_dir

    def format_dir_path(self, dir_path: Path) -> str:
        """format a dir path, just returns the path as str by default but can be
           redefined to point to __format_dir_path_with_color() if settings
           require colorization"""
        return str(dir_path)

    def __format_file_name_with_color(self, file_name: str) -> str:
        """format a file name, highlighting matches with color"""
        formatted_file_name = file_name
        for p in self.settings.in_file_patterns:
            match = p.search(formatted_file_name)
            if match:
                formatted_file_name = FileResultFormatter.colorize(
                    formatted_file_name, match.start(), match.end())
                break
        if len(self.settings.in_extensions):
            # we know that file_name has a matching extension, no need to match
            idx = formatted_file_name.rfind('.')
            if 0 < idx < len(formatted_file_name) - 1:
                formatted_file_name = FileResultFormatter.colorize(
                    formatted_file_name, idx + 1, len(formatted_file_name))
        return formatted_file_name

    def format_file_name(self, file_name: str) -> str:
        """format a file name, just returns the name by default but can be
           redefined to point to __format_file_name_with_color() if settings
           require colorization"""
        return file_name

    def format_path(self, path: Path) -> str:
        """format a path"""
        parent = '.'
        if path.parent:
            parent = self.format_dir_path(path.parent)
        file_name = self.format_file_name(path.name)
        return os.path.join(parent, file_name)

    def format_file_result(self, result: FileResult) -> str:
        """format a FileResult instance"""
        return self.format_path(result.path)
