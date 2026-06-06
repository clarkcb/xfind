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
from pathlib import Path
from typing import Optional

from .common import log
from .constants import *
from .fileresult import FileResult, FileResultFormatter, FileResultSorter
from .filetypes import FileType, FileTypes
from .fileutil import FileUtil
from .findexception import FindException
from .findsettings import FindSettings, PatternSet


class Finder:
    """Finder is a class to find files based on find settings."""

    __slots__ = ['settings', 'file_types', '_matching_dir_cache']

    def __init__(self, settings: FindSettings):
        """Create a new Finder instance."""
        self.settings = settings
        self.file_types = FileTypes()
        self._matching_dir_cache = set()
        self.__validate_settings()

    def __validate_settings(self):
        """Validate the required settings in the FindSettings instance."""
        assert len(self.settings.paths) > 0, START_PATH_NOT_DEFINED
        for p in self.settings.paths:
            if not p.exists():
                p = p.expanduser()
            assert p.exists(), START_PATH_NOT_FOUND
            assert os.access(p, os.R_OK), START_PATH_NOT_READABLE
            if p.is_symlink():
                assert self.settings.follow_symlinks, \
                    START_PATH_DOES_NOT_MATCH_FIND_SETTINGS
            elif p.is_dir():
                assert (self.is_traversable_dir_path(p)), \
                    START_PATH_DOES_NOT_MATCH_FIND_SETTINGS
            elif p.is_file():
                assert self.filter_to_file_result(p) is not None, \
                    START_PATH_DOES_NOT_MATCH_FIND_SETTINGS
            else:
                # TODO: start path is unknown/invalid type
                raise FindException(START_PATH_DOES_NOT_MATCH_FIND_SETTINGS)
        if self.settings.max_depth > -1 and self.settings.min_depth > -1:
            assert self.settings.max_depth >= self.settings.min_depth, \
                INVALID_RANGE_MINDEPTH_MAXDEPTH
        if self.settings.max_last_mod and self.settings.min_last_mod:
            assert self.settings.max_last_mod >= self.settings.min_last_mod, \
                INVALID_RANGE_MINLASTMOD_MAXLASTMOD
        if self.settings.max_size > 0 and self.settings.min_size > 0:
            assert self.settings.max_size >= self.settings.min_size, \
                INVALID_RANGE_MINSIZE_MAXSIZE

    def is_matching_path_by_symlink(self, path: Path) -> bool:
        """Check whether the given path matches symlink settings.
           If follow_symlinks is false, then the path must not be a symlink."""
        # NOTE: follow_symlinks is added to is_file and is_dir in python 3.13,
        #       for now we have to check is_symlink and follow_symlinks
        return self.settings.follow_symlinks or not path.is_symlink()

    def is_matching_dir_path_by_hidden(self, dir_path: Path) -> bool:
        """Check whether the given directory matches hidden settings.
           None and empty checks should be done separately"""
        return self.settings.include_hidden or not FileUtil.is_hidden_path(dir_path)

    def is_matching_dir_path_by_in_patterns(self, dir_path: Path) -> bool:
        """Check whether the given directory matches the "in" patterns.
           None and empty checks should be done separately"""
        return empty_or_any_matches_any_pattern(set(dir_path.parts), self.settings.in_dir_patterns)

    def is_matching_dir_path_by_out_patterns(self, dir_path: Path) -> bool:
        """Check whether the given directory does not match the "out" patterns.
           None and empty checks should be done separately"""
        return empty_or_not_any_matches_any_pattern(set(dir_path.parts), self.settings.out_dir_patterns)

    def is_traversable_dir_path(self, dir_path: Path) -> bool:
        """Check whether a directory should be traversed (hidden/out filters only)."""
        return self.is_matching_dir_path_by_hidden(dir_path) \
            and self.is_matching_dir_path_by_out_patterns(dir_path)

    def is_matching_dir_path(self, dir_path: Path) -> bool:
        """Check whether a traversable directory is included by in-patterns."""
        if dir_path in self._matching_dir_cache:
            return True
        if not self.is_traversable_dir_path(dir_path):
            return False
        if not self.is_matching_dir_path_by_in_patterns(dir_path):
            return False
        self._matching_dir_cache.add(dir_path)
        return True

    def is_null_or_matching_dir_path(self, dir_path: Optional[Path]) -> bool:
        """Check whether a directory is None or a match."""
        # null or empty is traversable, also dot dir (. and ..)
        if dir_path is None or str(dir_path) == '' or FileUtil.is_dot_dir_path(dir_path):
            return True
        return self.is_matching_dir_path(dir_path)

    def is_matching_file_name_by_hidden(self, file_name: str) -> bool:
        """Check whether the given file name matches hidden file settings."""
        return self.settings.include_hidden or not FileUtil.is_hidden_name(file_name)

    def is_matching_archive_extension(self, ext: str) -> bool:
        """Check whether the given archive extension matches find settings."""
        return _is_matching_extension_sets(ext, self.settings.in_archive_extensions,
                                           self.settings.out_archive_extensions)

    def is_matching_archive_extension_for_file_path(self, file_path: Path) -> bool:
        """Check whether extension derived from a path matches archive settings."""
        if self.settings.in_archive_extensions or self.settings.out_archive_extensions:
            ext = FileUtil.get_extension(file_path.name)
            return self.is_matching_archive_extension(ext)
        return True

    def is_matching_archive_file_name(self, file_name: str) -> bool:
        """Check whether the given file name matches find settings."""
        return (empty_or_matches_any_pattern(file_name, self.settings.in_archive_file_patterns)
                and empty_or_not_matches_any_pattern(file_name, self.settings.out_archive_file_patterns))

    def is_matching_archive_file_name_for_file_path(self, file_path: Path) -> bool:
        """Check whether file_name derived from a path matches archive settings."""
        if self.settings.in_archive_file_patterns or self.settings.out_archive_file_patterns:
            return self.is_matching_archive_file_name(file_path.name)
        return True

    def is_matching_archive_file_path(self, file_path: Path) -> bool:
        """Check whether file_name derived from a path matches archive settings."""
        return self.is_matching_archive_extension_for_file_path(file_path) \
            and self.is_matching_archive_file_name_for_file_path(file_path)

    def is_matching_archive_file_result(self, file_result: FileResult) -> bool:
        """Check whether the given archive file matches find settings."""
        if file_result.path is None:
            return False
        return self.is_matching_archive_file_path(file_result.path)


    def is_matching_extension(self, ext: str) -> bool:
        """Check whether the given extension matches find settings."""
        return _is_matching_extension_sets(ext, self.settings.in_extensions,
                                           self.settings.out_extensions)

    def is_matching_extension_for_file_path(self, file_path: Path) -> bool:
        """Check whether extension derived from a path matches file settings."""
        if self.settings.in_extensions or self.settings.out_extensions:
            ext = FileUtil.get_extension(file_path.name)
            return self.is_matching_extension(ext)
        return True

    def is_matching_file_name(self, file_name: str) -> bool:
        """Check whether the given file name matches find settings."""
        return (empty_or_matches_any_pattern(file_name, self.settings.in_file_patterns)
                and empty_or_not_matches_any_pattern(file_name, self.settings.out_file_patterns))

    def is_matching_file_name_for_file_path(self, file_path: Path) -> bool:
        """Check whether file_name derived from a path matches settings."""
        if self.settings.in_file_patterns or self.settings.out_file_patterns:
            return self.is_matching_file_name(file_path.name)
        return True

    def is_matching_file_path(self, file_path: Path) -> bool:
        """Check whether file_name derived from a path matches settings."""
        return self.is_matching_extension_for_file_path(file_path) \
            and self.is_matching_file_name_for_file_path(file_path)

    def is_matching_file_type(self, file_type: FileType) -> bool:
        """Check whether the given file type matches find settings."""
        return (empty_or_matches_any_file_type(file_type, self.settings.in_file_types)
                and empty_or_not_matches_any_file_type(file_type, self.settings.out_file_types))

    def is_matching_file_size(self, file_size: int) -> bool:
        """Check whether the given file size matches find settings."""
        return (self.settings.min_size == 0
                or file_size >= self.settings.min_size) \
            and (self.settings.max_size == 0
                 or file_size <= self.settings.max_size)

    def is_matching_last_mod(self, last_mod: float) -> bool:
        """Check whether the given file last mod matches find settings."""
        return (self.settings.min_last_mod is None
                or last_mod >= self.settings.min_last_mod.timestamp()) \
            and (self.settings.max_last_mod is None
                 or last_mod <= self.settings.max_last_mod.timestamp())

    def is_matching_file_result(self, file_result: FileResult) -> bool:
        """Check whether the given file result matches find settings."""
        if file_result.path is None:
            return False
        return self.is_matching_file_path(file_result.path) \
            and self.is_matching_file_type(file_result.file_type) \
            and self.is_matching_file_size(file_result.file_size) \
            and self.is_matching_last_mod(file_result.last_mod)

    def filter_archive_file_path_to_file_result(self, file_path: Path) -> Optional[FileResult]:
        """Return a FileResult instance if the given archive file_path matches find settings, else None."""
        if not self.settings.include_archives and not self.settings.archives_only:
            return None

        if not self.is_matching_archive_file_path(file_path):
            return None

        file_size = 0
        last_mod = 0.0
        return FileResult(path=file_path, file_type=FileType.ARCHIVE, file_size=file_size, last_mod=last_mod)

    def filter_reg_file_path_to_file_result(self, file_path: Path, file_type: FileType) -> Optional[FileResult]:
        """Return a FileResult instance if the given regular file_path matches find settings, else None."""
        if self.settings.archives_only:
            return None

        if not self.is_matching_file_path(file_path) or not self.is_matching_file_type(file_type):
            return None

        file_size = 0
        last_mod = 0.0
        if self.settings.need_size() or self.settings.need_last_mod():
            stat = file_path.stat()
            if self.settings.need_size():
                file_size = stat.st_size
            if self.settings.need_last_mod():
                last_mod = stat.st_mtime

            if not self.is_matching_file_size(file_size) or not self.is_matching_last_mod(last_mod):
                return None

        return FileResult(path=file_path, file_type=file_type, file_size=file_size, last_mod=last_mod)

    def filter_to_file_result(self, file_path: Path) -> Optional[FileResult]:
        """Return a FileResult instance if the given file_path matches find settings, else None."""
        if not self.is_null_or_matching_dir_path(file_path.parent):
            return None
        if not self.settings.include_hidden and FileUtil.is_hidden_name(file_path.name):
            return None
        file_type = self.file_types.get_file_type_for_path(file_path)
        if file_type == FileType.ARCHIVE:
            return self.filter_archive_file_path_to_file_result(file_path)
        return self.filter_reg_file_path_to_file_result(file_path, file_type)

    def _rec_get_file_results_for_dir_path(self, dir_path: Path, min_depth: int, max_depth: int,
                                           current_depth: int) -> list[FileResult]:
        """Recursively get file results for given file path matching min and max depth"""
        file_results = []
        recurse = True
        if current_depth == max_depth:
            recurse = False
        elif -1 < max_depth < current_depth:
            return file_results
        dirs = []
        for f in dir_path.iterdir():
            if not self.is_matching_path_by_symlink(f):
                continue
            if f.is_dir() and recurse and self.is_traversable_dir_path(f):
                dirs.append(f)
            elif f.is_file() and (min_depth < 0 or current_depth >= min_depth):
                fr = self.filter_to_file_result(f)
                if fr:
                    file_results.append(fr)
        for d in dirs:
            file_results.extend(self._rec_get_file_results_for_dir_path(d, min_depth, max_depth,
                                                                        current_depth + 1))
        return file_results

    def get_file_results_for_path(self, path: Path) -> list[FileResult]:
        """Get file results for given path."""
        if not path.exists():
            path = path.expanduser()
            if not path.exists():
                raise FindException(START_PATH_NOT_FOUND)
        if path.is_symlink() and not self.settings.follow_symlinks:
            raise FindException(START_PATH_DOES_NOT_MATCH_FIND_SETTINGS)
        if path.is_dir():
            # if max_depth is zero, we can skip since a directory cannot be a result
            if self.settings.max_depth == 0:
                return []
            if self.is_traversable_dir_path(path):
                max_depth = self.settings.max_depth
                if not self.settings.recursive:
                    max_depth = 1
                return self._rec_get_file_results_for_dir_path(path, self.settings.min_depth,
                                                               max_depth, 1)
            else:
                raise FindException(START_PATH_DOES_NOT_MATCH_FIND_SETTINGS)
        elif path.is_file():
            # if min_depth > zero, we can skip since the file is at depth zero
            if self.settings.min_depth > 0:
                return []
            fr = self.filter_to_file_result(path)
            if fr:
                return [fr]
            else:
                raise FindException(START_PATH_DOES_NOT_MATCH_FIND_SETTINGS)
        else:
            raise FindException(START_PATH_DOES_NOT_MATCH_FIND_SETTINGS)

    def find_files(self) -> list[FileResult]:
        """Get the list of all files matching find settings."""
        file_results = []
        for p in self.settings.paths:
            file_results.extend(self.get_file_results_for_path(p))
        return file_results

    async def find(self) -> list[FileResult]:
        """Find matching files under paths."""
        file_results = self.find_files()
        file_result_sorter = FileResultSorter(self.settings)
        return file_result_sorter.sort(file_results)


def matches_any_pattern(s: str, pattern_set: PatternSet) -> bool:
    """Return true if string s matches any pattern in pattern_set, else
       false."""
    return any(p.search(s) for p in pattern_set)


def empty_or_matches_any_pattern(s: str, pattern_set: PatternSet) -> bool:
    return not pattern_set or matches_any_pattern(s, pattern_set)


def empty_or_not_matches_any_pattern(s: str, pattern_set: PatternSet) -> bool:
    return not pattern_set or not matches_any_pattern(s, pattern_set)


def empty_or_any_matches_any_pattern(ss: set[str], pattern_set: PatternSet) -> bool:
    return not pattern_set or any(matches_any_pattern(s, pattern_set) for s in ss)


def empty_or_not_any_matches_any_pattern(ss: set[str], pattern_set: PatternSet) -> bool:
    return not pattern_set or not any(matches_any_pattern(s, pattern_set) for s in ss)


def empty_or_matches_any_string(s: str, strings: set[str]) -> bool:
    return not strings or s in strings


def empty_or_not_matches_any_string(s: str, strings: set[str]) -> bool:
    return not strings or s not in strings


def empty_or_matches_any_file_type(file_type: FileType, in_file_types: set[FileType]) -> bool:
    return not in_file_types or file_type in in_file_types


def empty_or_not_matches_any_file_type(file_type: FileType, out_file_types: set[FileType]) -> bool:
    return not out_file_types or file_type not in out_file_types


def _is_matching_extension_sets(ext: str, in_extensions: set[str], out_extensions: set[str]) -> bool:
    """Check extension against include/exclude extension sets."""
    return (empty_or_matches_any_string(ext, in_extensions)
            and empty_or_not_matches_any_string(ext, out_extensions))


def get_matching_dirs(file_results: list[FileResult]) -> list[Path]:
    """Get the list of matching directories from the file results."""
    return sorted(list({f.path.parent for f in file_results if f.path and f.path.parent}))


def print_matching_dirs(file_results: list[FileResult], formatter: FileResultFormatter):
    dirs = get_matching_dirs(file_results)
    if dirs:
        log(f'\nMatching directories ({len(dirs)}):')
        for d in dirs:
            log(formatter.format_dir_path(d))
    else:
        log('\nMatching directories: 0')


def print_matching_files(file_results: list[FileResult], formatter: FileResultFormatter):
    """Print the file results"""
    if file_results:
        log(f'\nMatching files ({len(file_results)}):')
        for f in file_results:
            log(formatter.format_file_result(f))
    else:
        log('\nMatching files: 0')

