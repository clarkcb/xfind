# -*- coding: utf-8 -*-
"""
###############################################################################
#
# findsettings.py
#
# class FindSettings: encapsulates find settings
#
###############################################################################
"""
from datetime import datetime
from io import StringIO
from pathlib import Path
import re
from enum import StrEnum
from typing import Any, Optional, Pattern

from .common import list_to_str
from .filetypes import FileType
from .findexception import FindException


PatternSet = set[Pattern]


class SortBy(StrEnum):
    """SortBy enum"""
    FILEPATH = 'filepath'
    FILENAME = 'filename'
    FILETYPE = 'filetype'
    FILESIZE = 'filesize'
    LASTMOD = 'lastmod'


def get_sort_by_for_name(sort_by_name: str):
    """Set sort-by from str"""
    sort_by_name = sort_by_name.strip().upper()
    match sort_by_name:
        case 'FILEPATH' | 'PATH':
            return SortBy.FILEPATH
        case 'FILENAME' | 'NAME':
            return SortBy.FILENAME
        case 'FILESIZE' | 'SIZE':
            return SortBy.FILESIZE
        case 'FILETYPE' | 'TYPE':
            return SortBy.FILETYPE
        case 'LASTMOD':
            return SortBy.LASTMOD
        case _:
            return SortBy.FILEPATH


class FindSettings:
    """a class to encapsulate find settings for a particular find session"""

    __slots__ = [
        'archives_only', 'debug', 'in_archive_extensions', 'in_archive_file_patterns',
        'in_dir_patterns', 'in_extensions', 'in_file_patterns', 'in_file_types',
        'include_archives', 'include_hidden', 'max_depth', 'max_last_mod', 'max_size',
        'min_depth', 'min_last_mod', 'min_size', 'out_archive_extensions',
        'out_archive_file_patterns', 'out_dir_patterns', 'out_extensions',
        'out_file_patterns', 'out_file_types', 'paths', 'print_dirs', 'print_files',
        'print_usage', 'print_version', 'recursive', 'sort_by', 'sort_case_insensitive',
        'sort_descending', 'verbose'
    ]

    def __init__(self,
                 archives_only: bool = False,
                 debug: bool = False,
                 in_archive_extensions: list[str] | set[str] | str = None,
                 in_archive_file_patterns: list | set | str | Pattern = None,
                 in_dir_patterns: list | set | str | Pattern = None,
                 in_extensions: list[str] | set[str] | str = None,
                 in_file_patterns: list | set | str | Pattern = None,
                 in_file_types: list | set | str | FileType = None,
                 include_archives: bool = False,
                 include_hidden: bool = False,
                 max_depth: int = -1,
                 max_last_mod: Optional[datetime] = None,
                 max_size: int = 0,
                 min_depth: int = -1,
                 min_last_mod: Optional[datetime] = None,
                 min_size: int = 0,
                 out_archive_extensions: list[str] | set[str] | str = None,
                 out_archive_file_patterns: list | set | str | Pattern = None,
                 out_dir_patterns: list | set | str | Pattern = None,
                 out_extensions: list[str] | set[str] | str = None,
                 out_file_patterns: list | set | str | Pattern = None,
                 out_file_types: list | set | str | FileType = None,
                 paths: list[str] | set[str] | str = None,
                 print_dirs: bool = False,
                 print_files: bool = False,
                 print_usage: bool = False,
                 print_version: bool = False,
                 recursive: bool = True,
                 sort_by: SortBy = SortBy.FILEPATH,
                 sort_case_insensitive: bool = False,
                 sort_descending: bool = False,
                 verbose: bool = False):
        self.archives_only = archives_only
        self.debug = debug
        self.in_archive_extensions: set[str] = set()
        if in_archive_extensions:
            self.add_strs_to_set(in_archive_extensions, 'in_archive_extensions')
        self.in_archive_file_patterns: PatternSet = set()
        if in_archive_file_patterns:
            self.add_patterns(in_archive_file_patterns, 'in_archive_file_patterns')
        self.in_dir_patterns: PatternSet = set()
        if in_dir_patterns:
            self.add_patterns(in_dir_patterns, 'in_dir_patterns')
        self.in_extensions: set[str] = set()
        if in_extensions:
            self.add_strs_to_set(in_extensions, 'in_extensions')
        self.in_file_patterns: PatternSet = set()
        if in_file_patterns:
            self.add_patterns(in_file_patterns, 'in_file_patterns')
        self.in_file_types: set[FileType] = set()
        if in_file_types:
            self.add_file_types(in_file_types, 'in_file_types')
        self.include_archives = include_archives
        self.include_hidden = include_hidden
        self.max_depth = max_depth
        self.max_last_mod = max_last_mod
        self.max_size = max_size
        self.min_depth = min_depth
        self.min_last_mod = min_last_mod
        self.min_size = min_size
        self.out_archive_extensions: set[str] = set()
        if out_archive_extensions:
            self.add_strs_to_set(out_archive_extensions, 'out_archive_extensions')
        self.out_archive_file_patterns: PatternSet = set()
        if out_archive_file_patterns:
            self.add_patterns(out_archive_file_patterns, 'out_archive_file_patterns')
        self.out_dir_patterns: PatternSet = set()
        if out_dir_patterns:
            self.add_patterns(out_dir_patterns, 'out_dir_patterns')
        self.out_extensions: set[str] = set()
        if out_extensions:
            self.add_strs_to_set(out_extensions, 'out_extensions')
        self.out_file_patterns: PatternSet = set()
        if out_file_patterns:
            self.add_patterns(out_file_patterns, 'out_file_patterns')
        self.out_file_types: set[FileType] = set()
        if out_file_types:
            self.add_file_types(out_file_types, 'out_file_types')
        self.paths: set[Path] = set()
        if paths:
            self.add_paths(paths)
        self.print_dirs = print_dirs
        self.print_files = print_files
        self.print_usage = print_usage
        self.print_version = print_version
        self.recursive = recursive
        self.sort_by = sort_by
        self.sort_case_insensitive = sort_case_insensitive
        self.sort_descending = sort_descending
        self.verbose = verbose

    def add_strs_to_set(self, strs: list[str] | set[str] | str, set_name: str):
        """Add one or more comma-separated strs to set"""
        if isinstance(strs, str):
            strs = {strs}
        if isinstance(strs, (list, set)):
            for s in strs:
                new_set = {x for x in s.split(',') if x}
                str_set = getattr(self, set_name)
                str_set.update(new_set)
        else:
            raise FindException('strs is an unknown type')

    def add_patterns(self, patterns: list | set | str | Pattern, pattern_set_name: str, compile_flag=re.S | re.U):
        """Add patterns to patternset"""
        if isinstance(patterns, (list, set)):
            pattern_set = getattr(self, pattern_set_name)
            if all(isinstance(p, Pattern) for p in patterns):
                pattern_set.update(patterns)
            else:  # assume all strings
                pattern_set.update({re.compile(p, compile_flag) for p in patterns})
        elif isinstance(patterns, str):
            pattern_set = getattr(self, pattern_set_name)
            pattern_set.add(re.compile(patterns, compile_flag))
        elif isinstance(patterns, Pattern):
            pattern_set = getattr(self, pattern_set_name)
            pattern_set.add(patterns)
        else:
            raise FindException('patterns is an unknown type')

    def add_paths(self, paths: list | set | str):
        """Add one or more paths"""
        if isinstance(paths, (list, set)):
            self.paths.update({Path(p) for p in paths})
        elif isinstance(paths, str):
            self.paths.add(Path(paths))
        else:
            raise FindException('paths is an unknown type')

    def add_path(self, path: str):
        """Add a single path"""
        self.paths.add(Path(path))

    def add_file_types(self, file_types: list | set | str | FileType, file_type_set_name: str):
        """Add one or more filetypes"""
        if isinstance(file_types, (list, set)):
            if all(isinstance(ft, FileType) for ft in file_types):
                new_file_type_set = set(file_types)
            else:  # assume all strings
                new_file_type_set = {FileType.from_name(ft) for ft in file_types}
        elif isinstance(file_types, str):
            new_file_type_set = {FileType.from_name(ft) for ft in file_types.split(',') if ft}
        elif isinstance(file_types, FileType):
            new_file_type_set = {file_types}
        else:
            raise FindException('file_types is an unknown type')
        file_type_set = getattr(self, file_type_set_name)
        file_type_set.update(new_file_type_set)

    def need_stat(self) -> bool:
        return self.sort_by == SortBy.FILESIZE or \
               self.sort_by == SortBy.LASTMOD or \
               self.max_last_mod or self.min_last_mod or \
               self.max_size > 0 or self.min_size > 0

    def set_property(self, name: str, val: Any):
        """Set a property"""
        setattr(self, name, val)
        # some trues trigger others
        if isinstance(val, bool) and val:
            if name == 'archives_only':
                self.include_archives = True
            elif name == 'debug':
                self.verbose = True

    def set_properties(self, propdict: dict[str, Any]):
        """Set properties"""
        for p in propdict.keys():
            self.set_property(p, propdict[p])

    def set_sort_by(self, sort_by_name: str):
        """Set sort-by from str"""
        self.sort_by = get_sort_by_for_name(sort_by_name)

    def __str__(self):
        sio = StringIO()
        sio.write(f'{self.__class__.__name__}(')
        for i, p in enumerate(sorted(self.__slots__)):
            if i > 0:
                sio.write(', ')
            sio.write(f'{p}=')
            val = getattr(self, p)
            if isinstance(val, set):
                if len(val) > 0:
                    if hasattr(list(val)[0], 'pattern'):
                        sio.write(list_to_str([x.pattern for x in val]))
                    elif isinstance(list(val)[0], FileType):
                        sio.write('[' + ', '.join([str(x) for x in val]) + ']')
                    else:
                        sio.write(list_to_str(list(val)))
                else:
                    sio.write('[]')
            elif isinstance(val, SortBy):
                sio.write(str(val))
            elif isinstance(val, str):
                if val:
                    sio.write(f'"{val}"')
                else:
                    sio.write('""')
            elif isinstance(val, Optional[datetime]):
                if val:
                    sio.write(f'"{val}"')
                else:
                    sio.write('0')
            else:
                sio.write(f'{val}')
        sio.write(')')
        return sio.getvalue()
