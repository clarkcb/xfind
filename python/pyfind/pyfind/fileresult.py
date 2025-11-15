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

from .color import ConsoleColor
from .filetypes import FileType
from .findsettings import FindSettings, SortBy


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
            ConsoleColor.GREEN + \
            s[match_start_index:match_end_index] + \
            ConsoleColor.RESET + \
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


class FileResultSorter(object):
    """provides sorting of FileResult instances"""

    def __init__(self, settings: FindSettings):
        self.settings = settings

    def _case(self, s: str) -> str:
        if self.settings.sort_case_insensitive:
            return s.casefold()
        return s

    def key_by_file_path(self, r: FileResult) -> list:
        return [[self._case(str(c)) for c in r.containers],
                self._case(str(r.path.parent)),
                self._case(r.path.name)]

    def key_by_file_name(self, r: FileResult) -> list:
        return [self._case(r.path.name),
                [self._case(str(c)) for c in r.containers],
                self._case(str(r.path.parent))]

    def key_by_file_size(self, r: FileResult) -> list:
        return [r.file_size] + self.key_by_file_path(r)

    def key_by_file_type(self, r: FileResult) -> list:
        return [r.file_type] + self.key_by_file_path(r)

    def key_by_last_mod(self, r: FileResult) -> list:
        return [r.last_mod] + self.key_by_file_path(r)

    def get_sort_key_function(self) -> callable:
        """Get the sort key function based on the settings."""
        match self.settings.sort_by:
            case SortBy.FILEPATH:
                return self.key_by_file_path
            case SortBy.FILENAME:
                return self.key_by_file_name
            case SortBy.FILESIZE:
                return self.key_by_file_size
            case SortBy.FILETYPE:
                return self.key_by_file_type
            case SortBy.LASTMOD:
                return self.key_by_last_mod
            case _:
                return self.key_by_file_path

    def sort(self, file_results: list[FileResult]) -> list[FileResult]:
        """Sort the given list of FileResult instances."""
        sort_key_func = self.get_sort_key_function()
        return sorted(file_results, key=sort_key_func,
                      reverse=self.settings.sort_descending)
