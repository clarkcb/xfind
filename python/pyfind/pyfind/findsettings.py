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
import re
from enum import Enum
from typing import Any, Optional, Pattern

from .filetypes import FileType
from .findexception import FindException

PatternSet = set[Pattern]


class SortBy(Enum):
    """SortBy enum"""
    FILEPATH = 0
    FILENAME = 1
    FILETYPE = 2
    FILESIZE = 3
    LASTMOD = 4


class FindSettings:
    """a class to encapsulate find settings for a particular find session"""

    __slots__ = [
        'archives_only', 'debug', 'exclude_hidden', 'in_archive_extensions',
        'in_archive_file_patterns', 'in_dir_patterns', 'in_extensions', 'in_file_patterns',
        'in_file_types', 'include_archives', 'list_dirs', 'list_files', 'max_last_mod',
        'max_depth', 'max_size', 'min_last_mod', 'min_depth', 'min_size',
        'out_archive_file_patterns', 'out_archive_extensions', 'out_dir_patterns',
        'out_extensions', 'out_file_patterns', 'out_file_types', 'paths', 'print_results',
        'print_usage', 'print_version', 'recursive', 'sort_by', 'sort_case_insensitive',
        'sort_descending', 'verbose'
    ]

    def __init__(self,
                 archives_only: bool = False,
                 debug: bool = False,
                 exclude_hidden: bool = True,
                 in_archive_extensions: set[str] = None,
                 in_archive_file_patterns: PatternSet = None,
                 in_dir_patterns: PatternSet = None,
                 in_extensions: set[str] = None,
                 in_file_patterns: PatternSet = None,
                 in_file_types: set[str] = None,
                 include_archives: bool = False,
                 list_dirs: bool = False,
                 list_files: bool = False,
                 max_depth: int = -1,
                 max_last_mod: Optional[datetime] = None,
                 max_size: int = 0,
                 min_depth: int = -1,
                 min_last_mod: Optional[datetime] = None,
                 min_size: int = 0,
                 out_archive_extensions: set[str] = None,
                 out_archive_file_patterns: PatternSet = None,
                 out_dir_patterns: PatternSet = None,
                 out_extensions: set[str] = None,
                 out_file_patterns: PatternSet = None,
                 out_file_types: set[str] = None,
                 paths: set[str] = None,
                 print_results: bool = False,
                 print_usage: bool = False,
                 print_version: bool = False,
                 recursive: bool = True,
                 sort_by: SortBy = SortBy.FILEPATH,
                 sort_case_insensitive: bool = False,
                 sort_descending: bool = False,
                 verbose: bool = False):
        self.archives_only = archives_only
        self.debug = debug
        self.exclude_hidden = exclude_hidden
        self.in_archive_extensions = in_archive_extensions if in_archive_extensions else set()
        self.in_archive_file_patterns: PatternSet = \
            in_archive_file_patterns if in_archive_file_patterns else set()
        self.in_dir_patterns: PatternSet = in_dir_patterns if in_dir_patterns else set()
        self.in_extensions = in_extensions if in_extensions else set()
        self.in_file_patterns: PatternSet = in_file_patterns if in_file_patterns else set()
        self.in_file_types = in_file_types if in_file_types else set()
        self.include_archives = include_archives
        self.list_dirs = list_dirs
        self.list_files = list_files
        self.max_depth = max_depth
        self.max_last_mod = max_last_mod
        self.max_size = max_size
        self.min_depth = min_depth
        self.min_last_mod = min_last_mod
        self.min_size = min_size
        self.out_archive_extensions = out_archive_extensions if out_archive_extensions else set()
        self.out_archive_file_patterns: PatternSet = \
            out_archive_file_patterns if out_archive_file_patterns else set()
        self.out_dir_patterns: PatternSet = out_dir_patterns if out_dir_patterns else set()
        self.out_extensions = out_extensions if out_extensions else set()
        self.out_file_patterns: PatternSet = out_file_patterns if out_file_patterns else set()
        self.out_file_types = out_file_types if out_file_types else set()
        self.paths = paths if paths else set()
        self.print_results = print_results
        self.print_usage = print_usage
        self.print_version = print_version
        self.recursive = recursive
        self.sort_by = sort_by
        self.sort_case_insensitive = sort_case_insensitive
        self.sort_descending = sort_descending
        self.verbose = verbose

    def add_exts(self, exts: list | set | str, ext_set_name: str):
        """Add one or more comma-separated extensions"""
        if isinstance(exts, (list, set)):
            ext_set = getattr(self, ext_set_name)
            ext_set.update(exts)
        elif isinstance(exts, str):
            new_ext_set = {ext for ext in exts.split(',') if ext}
            ext_set = getattr(self, ext_set_name)
            ext_set.update(new_ext_set)
        else:
            raise FindException('exts is an unknown type')

    def add_patterns(self, patterns: list | set | str, pattern_set_name: str, compile_flag=re.S | re.U):
        """Add patterns to patternset"""
        if isinstance(patterns, (list, set)):
            new_pattern_set = {re.compile(p, compile_flag) for p in patterns}
            pattern_set = getattr(self, pattern_set_name)
            pattern_set.update(new_pattern_set)
        elif isinstance(patterns, str):
            pattern_set = getattr(self, pattern_set_name)
            pattern_set.add(re.compile(patterns, compile_flag))
        else:
            raise FindException('patterns is an unknown type')

    def add_paths(self, paths):
        """Add one or more paths"""
        if isinstance(paths, (list, set)):
            self.paths.update(paths)
        elif isinstance(paths, str):
            self.paths.add(paths)
        else:
            raise FindException('paths is an unknown type')

    def add_file_types(self, file_types: list | set | str, file_type_set_name: str):
        """Add one or more filetypes"""
        if isinstance(file_types, (list, set)):
            new_file_type_set = {FileType.from_name(ft) for ft in file_types}
        elif isinstance(file_types, str):
            new_file_type_set = {FileType.from_name(ft) for ft in file_types.split(',') if ft}
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
        """Set sort-by"""
        match sort_by_name.strip().upper():
            case 'LASTMOD':
                self.sort_by = SortBy.LASTMOD
            case 'NAME':
                self.sort_by = SortBy.FILENAME
            case 'SIZE':
                self.sort_by = SortBy.FILESIZE
            case 'TYPE':
                self.sort_by = SortBy.FILETYPE
            case _:
                self.sort_by = SortBy.FILEPATH

    def __str__(self):
        print_dict = {}
        s = f'{self.__class__.__name__}('
        for p in sorted(self.__slots__):
            val = getattr(self, p)
            if isinstance(val, set):
                if len(val) > 0 and hasattr(list(val)[0], 'pattern'):
                    print_dict[p] = str([x.pattern for x in val])
                else:
                    print_dict[p] = str(list(val))
            elif isinstance(val, str):
                if val:
                    print_dict[p] = f'"{val}"'
                else:
                    print_dict[p] = '""'
            elif isinstance(val, Optional[datetime]):
                if val:
                    print_dict[p] = f'"{val}"'
                else:
                    print_dict[p] = '0'
            else:
                print_dict[p] = f'{val}'
        next_elem = 0
        for p in sorted(print_dict.keys()):
            if next_elem:
                s += ', '
            s += f'{p}: {print_dict[p]}'
            next_elem += 1
        s += ')'
        return s
