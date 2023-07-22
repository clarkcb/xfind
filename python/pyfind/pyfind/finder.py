# -*- coding: utf-8 -*-
"""
###############################################################################
#
# finder.py
#
# class Finder: executes a file find
#
###############################################################################
"""
import os
from typing import List, Optional

from .fileresult import FileResult
from .filetypes import FileType, FileTypes
from .fileutil import FileUtil
from .findsettings import FindSettings, PatternSet, SortBy


class Finder:
    """Finder is a class to find files based on find settings."""

    __slots__ = ['settings', 'file_types']

    def __init__(self, settings: FindSettings):
        """Create a new Finder instance."""
        self.settings = settings
        self.__validate_settings()
        self.file_types = FileTypes()

    def __validate_settings(self):
        """Validate the required settings in the FindSettings instance."""
        assert len(self.settings.paths) > 0, 'Startpath not defined'
        for p in self.settings.paths:
            assert os.path.exists(p), 'Startpath not found'
            assert os.access(p, os.R_OK), 'Startpath not readable'
        if self.settings.max_depth > -1 and self.settings.min_depth > -1:
            assert self.settings.max_depth >= self.settings.min_depth, \
                'Invalid range for mindepth and maxdepth'
        if self.settings.max_last_mod and self.settings.min_last_mod:
            assert self.settings.max_last_mod > self.settings.min_last_mod, \
                'Invalid range for min_last_mod and max_last_mod'
        if self.settings.max_size > 0 and self.settings.min_size > 0:
            assert self.settings.max_size > self.settings.min_size, \
                'Invalid range for min_size and max_size'

    def is_matching_dir(self, d: str) -> bool:
        """Check whether the given directory matches find settings."""
        path_elems = FileUtil.path_elems(d)
        if self.settings.exclude_hidden:
            for p in path_elems:
                if FileUtil.is_hidden(p):
                    return False
        if self.settings.in_dir_patterns and \
                not any_matches_any_pattern(path_elems, self.settings.in_dir_patterns):
            return False
        if self.settings.out_dir_patterns and \
                any_matches_any_pattern(path_elems, self.settings.out_dir_patterns):
            return False
        return True

    def is_matching_stat(self, stat: os.stat_result) -> bool:
        """Check whether the given file stat matches find settings."""
        if (self.settings.min_last_mod
            and stat.st_mtime < self.settings.min_last_mod.timestamp()) \
                or (self.settings.max_last_mod
                    and stat.st_mtime > self.settings.max_last_mod.timestamp()):
            return False
        if (self.settings.min_size and stat.st_size < self.settings.min_size) \
                or (self.settings.max_size and stat.st_size > self.settings.max_size):
            return False
        return True

    def is_matching_archive_file(self, file_name: str, stat: os.stat_result) -> bool:
        """Check whether the given archive file matches find settings."""
        ext = FileUtil.get_extension(file_name)
        if (self.settings.in_archive_extensions
            and ext not in self.settings.in_archive_extensions) \
            or (self.settings.out_archive_extensions
                and ext in self.settings.out_archive_extensions) \
            or (self.settings.in_archive_file_patterns
                and not matches_any_pattern(file_name, self.settings.in_archive_file_patterns)) \
            or (self.settings.out_archive_file_patterns
                and matches_any_pattern(file_name, self.settings.out_archive_file_patterns)):
            return False
        return self.is_matching_stat(stat)

    def is_matching_file(self, file_name: str, file_type: FileType, stat: os.stat_result) -> bool:
        """Check whether the given file matches find settings."""
        if self.settings.in_extensions or self.settings.out_extensions:
            ext = FileUtil.get_extension(file_name)
            if (self.settings.in_extensions and ext not in self.settings.in_extensions) \
                    or (self.settings.out_extensions and ext in self.settings.out_extensions):
                return False
        if (self.settings.in_file_patterns and
                not matches_any_pattern(file_name, self.settings.in_file_patterns)) \
                or (self.settings.out_file_patterns and
                    matches_any_pattern(file_name, self.settings.out_file_patterns)) \
                or (self.settings.in_file_types and file_type not in self.settings.in_file_types) \
                or (self.settings.out_file_types and file_type in self.settings.out_file_types):
            return False
        if stat:
            return self.is_matching_stat(stat)
        return True

    def filter_to_file_result(self, file_path: str) -> Optional[FileResult]:
        """Return a FileResult instance if the given file_path matches find settings, else None."""
        (path, file_name) = os.path.split(file_path)
        if self.settings.exclude_hidden and FileUtil.is_hidden(file_name):
            return None
        file_type = self.file_types.get_file_type(file_name)
        if file_type == FileType.ARCHIVE \
           and not self.settings.include_archives \
           and not self.settings.archives_only:
            return None
        stat = None
        if self.settings.need_stat():
            stat = os.stat(file_path)
        if file_type == FileType.ARCHIVE:
            if not self.is_matching_archive_file(file_name, stat):
                return None
        elif self.settings.archives_only or not self.is_matching_file(file_name, file_type, stat):
            return None
        return FileResult(path=path, file_name=file_name, file_type=file_type, stat=stat)

    def find_files(self) -> List[FileResult]:
        """Get the list of all files matching find settings."""
        file_results = []
        for p in self.settings.paths:
            if os.path.isdir(p):
                if self.is_matching_dir(os.path.abspath(p)):
                    if self.settings.recursive:
                        # TODO: add follow_symlinks to FindSettings and set here
                        for root, dirs, files in os.walk(p, topdown=True, followlinks=False):
                            if self.settings.max_depth > -1 or self.settings.min_depth > -1:
                                root_elem_count = len(FileUtil.path_elems(root))
                                path_elem_count = len(FileUtil.path_elems(p))
                                # If max_depth is defined, once reached, delete dirs
                                if self.settings.max_depth > -1:
                                    if root_elem_count - path_elem_count == self.settings.max_depth:
                                        while dirs:
                                            del(dirs[0])
                                # If min_depth is defined, if below, continue
                                if self.settings.min_depth > -1:
                                    if root_elem_count - path_elem_count < self.settings.min_depth:
                                        continue
                            # NOTE: skipping self.is_matching_dir(root) and checking dirs,
                            #       this has the effect of limiting checks to dirs
                            #       and removing duplicate checks of settings.paths
                            del_dirs = [d for d in dirs if not self.is_matching_dir(d)]
                            for d in del_dirs:
                                i = dirs.index(d)
                                del dirs[i]

                            # TODO: add option to follow symlinks? (skipping for now)
                            files = [
                                os.path.join(root, f) for f in files
                                if not os.path.islink(os.path.join(root, f))
                            ]
                            new_file_results = [self.filter_to_file_result(f) for f in files]
                            file_results.extend([fr for fr in new_file_results if fr])
                    else:
                        files = [
                            os.path.join(p, f) for f in os.listdir(p)
                            if os.path.isfile(os.path.join(p, f))
                            and not os.path.islink(os.path.join(p, f))
                        ]
                        new_file_results = [self.filter_to_file_result(f) for f in files]
                        file_results.extend([fr for fr in new_file_results if fr])
            elif os.path.isfile(p):
                fr = self.filter_to_file_result(p)
                if fr:
                    file_results.append(fr)
        return file_results

    async def find(self) -> List[FileResult]:
        """Find matching files under paths."""
        return self.sort_file_results(self.find_files())

    def sort_file_results(self, file_results: list[FileResult]) -> list[FileResult]:
        def c(s: str) -> str:
            if self.settings.sort_case_insensitive:
                return s.lower()
            return s
        match self.settings.sort_by:
            case SortBy.FILEPATH:
                return sorted(file_results, key=lambda r: (c(r.path), c(r.file_name)),
                              reverse=self.settings.sort_descending)
            case SortBy.FILENAME:
                return sorted(file_results, key=lambda r: (c(r.file_name), c(r.path)),
                              reverse=self.settings.sort_descending)
            case SortBy.FILESIZE:
                return sorted(file_results, key=lambda r: (r.stat.st_size, c(r.path), c(r.file_name)),
                              reverse=self.settings.sort_descending)
            case SortBy.FILETYPE:
                return sorted(file_results, key=lambda r: (r.file_type, c(r.path), c(r.file_name)),
                              reverse=self.settings.sort_descending)
            case SortBy.LASTMOD:
                return sorted(file_results, key=lambda r: (r.stat.st_mtime, c(r.path), c(r.file_name)),
                              reverse=self.settings.sort_descending)
            case _:
                return sorted(file_results, key=lambda r: (c(r.path), c(r.file_name)),
                              reverse=self.settings.sort_descending)


def matches_any_pattern(s: str, pattern_set: PatternSet) -> bool:
    """Return true if string s matches any pattern in pattern_set, else
       false."""
    return any(p.search(s) for p in pattern_set)


def any_matches_any_pattern(slist, pattern_set: PatternSet) -> bool:
    """Return true if any string in slist matches any pattern in
       pattern_set, else false."""
    for s in slist:
        if matches_any_pattern(s, pattern_set):
            return True
    return False
