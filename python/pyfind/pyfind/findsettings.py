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
from typing import Any, Dict, Optional, Pattern, Set

from .filetypes import FileType
from .findexception import FindException

PatternSet = Set[Pattern]


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
        'archivesonly', 'debug', 'excludehidden', 'in_archiveextensions',
        'in_archivefilepatterns', 'in_dirpatterns', 'in_extensions', 'in_filepatterns',
        'in_filetypes', 'includearchives', 'listdirs', 'listfiles', 'maxlastmod',
        'maxsize', 'minlastmod', 'minsize', 'out_archivefilepatterns', 'out_archiveextensions',
        'out_dirpatterns', 'out_extensions', 'out_filepatterns', 'out_filetypes', 'paths',
        'printresults', 'printusage', 'printversion', 'recursive', 'sortby', 'sort_descending',
        'verbose'
    ]

    def __init__(self, archivesonly: bool = False, debug: bool = False,
                 excludehidden: bool = True, in_archiveextensions: Set[str] = None,
                 in_archivefilepatterns: PatternSet = None, in_dirpatterns: PatternSet = None,
                 in_extensions: Set[str] = None, in_filepatterns: PatternSet = None,
                 in_filetypes: Set[str] = None, includearchives: bool = False,
                 listdirs: bool = False, listfiles: bool = False,
                 maxlastmod: Optional[datetime] = None, maxsize: int = 0,
                 minlastmod: Optional[datetime] = None, minsize: int = 0,
                 out_archiveextensions: Set[str] = None,
                 out_archivefilepatterns: PatternSet = None,
                 out_dirpatterns: PatternSet = None, out_extensions: Set[str] = None,
                 out_filepatterns: PatternSet = None, out_filetypes: Set[str] = None,
                 paths: Set[str] = None, printresults: bool = False, printusage: bool = False,
                 printversion: bool = False, recursive: bool = True,
                 sortby: SortBy = SortBy.FILEPATH, sort_descending: bool = False,
                 verbose: bool = False):
        self.archivesonly = archivesonly
        self.debug = debug
        self.excludehidden = excludehidden
        self.in_archiveextensions = in_archiveextensions if in_archiveextensions else set()
        self.in_archivefilepatterns: PatternSet = in_archivefilepatterns if in_archivefilepatterns else set()
        self.in_dirpatterns: PatternSet = in_dirpatterns if in_dirpatterns else set()
        self.in_extensions = in_extensions if in_extensions else set()
        self.in_filepatterns: PatternSet = in_filepatterns if in_filepatterns else set()
        self.in_filetypes = in_filetypes if in_filetypes else set()
        self.includearchives = includearchives
        self.listdirs = listdirs
        self.listfiles = listfiles
        self.maxlastmod = maxlastmod
        self.maxsize = maxsize
        self.minlastmod = minlastmod
        self.minsize = minsize
        self.out_archiveextensions = out_archiveextensions if out_archiveextensions else set()
        self.out_archivefilepatterns: PatternSet = out_archivefilepatterns if out_archivefilepatterns else set()
        self.out_dirpatterns: PatternSet = out_dirpatterns if out_dirpatterns else set()
        self.out_extensions = out_extensions if out_extensions else set()
        self.out_filepatterns: PatternSet = out_filepatterns if out_filepatterns else set()
        self.out_filetypes = out_filetypes if out_filetypes else set()
        self.paths = paths if paths else set()
        self.printresults = printresults
        self.printusage = printusage
        self.printversion = printversion
        self.recursive = recursive
        self.sortby = sortby
        self.sort_descending = sort_descending
        self.verbose = verbose

    def add_exts(self, exts, ext_set_name: str):
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

    def add_patterns(self, patterns, pattern_set_name: str, compile_flag=re.S | re.U):
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

    def add_filetypes(self, filetypes, filetype_set_name: str):
        """Add one or more filetypes"""
        if isinstance(filetypes, (list, set)):
            new_filetype_set = {FileType.from_name(ft) for ft in filetypes}
        elif isinstance(filetypes, str):
            new_filetype_set = {FileType.from_name(ft) for ft in filetypes.split(',') if ft}
        else:
            raise FindException('filetypes is an unknown type')
        filetype_set = getattr(self, filetype_set_name)
        filetype_set.update(new_filetype_set)

    def set_property(self, name: str, val):
        """Set a property"""
        setattr(self, name, val)
        # some trues trigger others
        if isinstance(val, bool) and val:
            if name == 'archivesonly':
                self.includearchives = True
            elif name == 'debug':
                self.verbose = True

    def set_properties(self, propdict: Dict[str, Any]):
        """Set properties"""
        for p in propdict.keys():
            self.set_property(p, propdict[p])

    def set_sort_by(self, sort_by_name: str):
        """Set sort-by"""
        match sort_by_name.strip().upper():
            case 'LASTMOD':
                self.sortby = SortBy.LASTMOD
            case 'NAME':
                self.sortby = SortBy.FILENAME
            case 'SIZE':
                self.sortby = SortBy.FILESIZE
            case 'TYPE':
                self.sortby = SortBy.FILETYPE
            case _:
                self.sortby = SortBy.FILEPATH

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
