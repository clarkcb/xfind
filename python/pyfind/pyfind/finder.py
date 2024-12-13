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

from .fileresult import FileResult
from .filetypes import FileType, FileTypes
from .fileutil import FileUtil
from .findexception import FindException
from .findsettings import FindSettings, PatternSet, SortBy


class Finder:
    """Finder is a class to find files based on find settings."""

    __slots__ = ['settings', 'file_types', '__matching_dir_cache']

    def __init__(self, settings: FindSettings):
        """Create a new Finder instance."""
        self.settings = settings
        self.__validate_settings()
        self.file_types = FileTypes()
        self.__matching_dir_cache = set([])

    def __validate_settings(self):
        """Validate the required settings in the FindSettings instance."""
        assert len(self.settings.paths) > 0, 'Startpath not defined'
        for p in self.settings.paths:
            assert p.exists() or p.expanduser().exists(), 'Startpath not found'
            assert os.access(p, os.R_OK) or os.access(p.expanduser(), os.R_OK), \
                'Startpath not readable'
        if self.settings.max_depth > -1 and self.settings.min_depth > -1:
            assert self.settings.max_depth >= self.settings.min_depth, \
                'Invalid range for mindepth and maxdepth'
        if self.settings.max_last_mod and self.settings.min_last_mod:
            assert self.settings.max_last_mod >= self.settings.min_last_mod, \
                'Invalid range for minlastmod and maxlastmod'
        if self.settings.max_size > 0 and self.settings.min_size > 0:
            assert self.settings.max_size >= self.settings.min_size, \
                'Invalid range for minsize and maxsize'

    def is_matching_dir(self, d: str | Path) -> bool:
        """Check whether the given directory matches find settings."""
        if isinstance(d, str):
            d = Path(d)
        if d in self.__matching_dir_cache:
            return True
        if not self.settings.include_hidden:
            if any(FileUtil.is_hidden(p) for p in d.parts):
                return False
        if self.settings.in_dir_patterns and \
                not any_matches_any_pattern(d.parts, self.settings.in_dir_patterns):
            return False
        if self.settings.out_dir_patterns and \
                any_matches_any_pattern(d.parts, self.settings.out_dir_patterns):
            return False
        self.__matching_dir_cache.add(d)
        return True

    def is_matching_archive_ext(self, ext: str) -> bool:
        """Check whether the given extension matches find settings."""
        return (not self.settings.in_archive_extensions
                or ext in self.settings.in_archive_extensions) \
            and (not self.settings.out_archive_extensions
                 or ext not in self.settings.out_archive_extensions)

    def is_matching_ext(self, ext: str) -> bool:
        """Check whether the given extension matches find settings."""
        return (not self.settings.in_extensions
                or ext in self.settings.in_extensions) \
            and (not self.settings.out_extensions
                 or ext not in self.settings.out_extensions)

    def has_matching_archive_ext(self, file_result: FileResult) -> bool:
        """Check whether the given extension matches find settings."""
        if self.settings.in_archive_extensions or self.settings.out_archive_extensions:
            ext = FileUtil.get_extension(file_result.path.name)
            return self.is_matching_archive_ext(ext)
        return True

    def has_matching_ext(self, file_result: FileResult) -> bool:
        """Check whether the given extension matches find settings."""
        if self.settings.in_extensions or self.settings.out_extensions:
            ext = FileUtil.get_extension(file_result.path.name)
            return self.is_matching_ext(ext)
        return True

    def is_matching_archive_file_name(self, file_name: str) -> bool:
        """Check whether the given file name matches find settings."""
        return (not self.settings.in_archive_file_patterns
                or matches_any_pattern(file_name, self.settings.in_archive_file_patterns)) \
            and (not self.settings.out_archive_file_patterns
                 or not matches_any_pattern(file_name, self.settings.out_archive_file_patterns))

    def is_matching_file_name(self, file_name: str) -> bool:
        """Check whether the given file name matches find settings."""
        return (not self.settings.in_file_patterns
                or matches_any_pattern(file_name, self.settings.in_file_patterns)) \
            and (not self.settings.out_file_patterns
                 or not matches_any_pattern(file_name, self.settings.out_file_patterns))

    def is_matching_file_type(self, file_type: FileType) -> bool:
        """Check whether the given file type matches find settings."""
        return (not self.settings.in_file_types
                or file_type in self.settings.in_file_types) \
            and (not self.settings.out_file_types
                 or file_type not in self.settings.out_file_types)

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

    def is_matching_archive_file_result(self, file_result: FileResult) -> bool:
        """Check whether the given archive file matches find settings."""
        return self.has_matching_archive_ext(file_result) \
            and self.is_matching_archive_file_name(file_result.path.name)

    def is_matching_file_result(self, file_result: FileResult) -> bool:
        """Check whether the given file matches find settings."""
        return self.has_matching_ext(file_result) \
            and self.is_matching_file_name(file_result.path.name) \
            and self.is_matching_file_type(file_result.file_type) \
            and self.is_matching_file_size(file_result.file_size) \
            and self.is_matching_last_mod(file_result.last_mod)

    def filter_to_file_result(self, file_path: Path) -> Optional[FileResult]:
        """Return a FileResult instance if the given file_path matches find settings, else None."""
        if not self.settings.include_hidden and FileUtil.is_hidden(file_path.name):
            return None
        file_type = self.file_types.get_file_type_for_path(file_path)
        if file_type == FileType.ARCHIVE \
                and not self.settings.include_archives \
                and not self.settings.archives_only:
            return None
        file_size = 0
        last_mod = 0.0
        if self.settings.need_size() or self.settings.need_last_mod():
            stat = file_path.stat()
            if self.settings.need_size():
                file_size = stat.st_size
            if self.settings.need_last_mod():
                last_mod = stat.st_mtime
        file_result = FileResult(path=file_path, file_type=file_type, file_size=file_size,
                                 last_mod=last_mod)
        if file_type == FileType.ARCHIVE:
            if self.is_matching_archive_file_result(file_result):
                return file_result
            return None
        if not self.settings.archives_only and self.is_matching_file_result(file_result):
            return file_result
        return None

    def _rec_get_file_results_for_path(self, file_path: Path, min_depth: int, max_depth: int,
                                       current_depth: int) -> list[FileResult]:
        """Recursively get file results for given file path matching min and max depth"""
        file_results = []
        recurse = True
        if current_depth == max_depth:
            recurse = False
        elif -1 < max_depth < current_depth:
            return file_results
        dirs = []
        for f in file_path.iterdir():
            # NOTE: follow_symlinks is added to is_file and is_dir in python 3.13,
            #       for now we have to check is_symlink and follow_symlinks
            if f.is_symlink() and not self.settings.follow_symlinks:
                continue
            # if f.is_dir(follow_symlinks=self.settings.follow_symlinks) and recurse and self.is_matching_dir(f):
            if f.is_dir() and recurse and self.is_matching_dir(f):
                dirs.append(f)
            elif f.is_file() and (min_depth < 0 or current_depth >= min_depth):
                fr = self.filter_to_file_result(f)
                if fr:
                    file_results.append(fr)
        for d in dirs:
            file_results.extend(self._rec_get_file_results_for_path(d, min_depth, max_depth,
                                                                    current_depth + 1))
        return file_results

    def get_file_results_for_path(self, file_path: Path) -> list[FileResult]:
        """Get file results for given file path."""
        if not file_path.exists():
            file_path = file_path.expanduser()
        if file_path.is_dir():
            # if max_depth is zero, we can skip since a directory cannot be a result
            if self.settings.max_depth == 0:
                return []
            if self.is_matching_dir(file_path):
                max_depth = self.settings.max_depth
                if not self.settings.recursive:
                    max_depth = 1
                return self._rec_get_file_results_for_path(file_path, self.settings.min_depth,
                                                           max_depth, 1)
            else:
                raise FindException('Startpath does not match find settings')
        elif file_path.is_file():
            # if min_depth > zero, we can skip since the file is at depth zero
            if self.settings.min_depth > 0:
                return []
            fr = self.filter_to_file_result(file_path)
            if fr:
                return [fr]
            else:
                raise FindException('Startpath does not match find settings')

    def find_files(self) -> list[FileResult]:
        """Get the list of all files matching find settings."""
        file_results = []
        for p in self.settings.paths:
            file_results.extend(self.get_file_results_for_path(p))
        return file_results

    async def find(self) -> list[FileResult]:
        """Find matching files under paths."""
        return self.sort_file_results(self.find_files())

    def case(self, s: str) -> str:
        if self.settings.sort_case_insensitive:
            return s.casefold()
        return s

    def key_by_file_path(self, r: FileResult):
        return [[self.case(str(c)) for c in r.containers],
                self.case(str(r.path.parent)),
                self.case(r.path.name)]

    def key_by_file_name(self, r: FileResult):
        return [self.case(r.path.name),
                [self.case(str(c)) for c in r.containers],
                self.case(str(r.path.parent))]

    def key_by_file_size(self, r: FileResult):
        return [r.file_size] + self.key_by_file_path(r)

    def key_by_file_type(self, r: FileResult):
        return [r.file_type] + self.key_by_file_path(r)

    def key_by_last_mod(self, r: FileResult):
        return [r.last_mod] + self.key_by_file_path(r)

    def sort_file_results(self, file_results: list[FileResult]) -> list[FileResult]:
        """Sort the given list of FileResult instances."""
        match self.settings.sort_by:
            case SortBy.FILEPATH:
                return sorted(file_results, key=self.key_by_file_path,
                              reverse=self.settings.sort_descending)
            case SortBy.FILENAME:
                return sorted(file_results, key=self.key_by_file_name,
                              reverse=self.settings.sort_descending)
            case SortBy.FILESIZE:
                return sorted(file_results, key=self.key_by_file_size,
                              reverse=self.settings.sort_descending)
            case SortBy.FILETYPE:
                return sorted(file_results, key=self.key_by_file_type,
                              reverse=self.settings.sort_descending)
            case SortBy.LASTMOD:
                return sorted(file_results, key=self.key_by_last_mod,
                              reverse=self.settings.sort_descending)
            case _:
                return sorted(file_results, key=self.key_by_file_path,
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
