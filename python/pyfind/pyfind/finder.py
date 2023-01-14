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

    __slots__ = ['settings', 'filetypes']

    def __init__(self, settings: FindSettings):
        """Create a new Finder instance."""
        self.settings = settings
        self.__validate_settings()
        self.filetypes = FileTypes()

    def __validate_settings(self):
        """Validate the required settings in the FindSettings instance."""
        assert len(self.settings.paths) > 0, 'Startpath not defined'
        for p in self.settings.paths:
            assert os.path.exists(p), 'Startpath not found'
            assert os.access(p, os.R_OK), 'Startpath not readable'
        if self.settings.maxlastmod and self.settings.minlastmod:
            assert self.settings.maxlastmod > self.settings.minlastmod, \
                'Invalid range for minlastmod and maxlastmod'
        if self.settings.maxsize > 0 and self.settings.minsize > 0:
            assert self.settings.maxsize > self.settings.minsize, \
                'Invalid range for minsize and maxsize'

    def is_matching_dir(self, d: str) -> bool:
        """Check whether the given directory matches find settings."""
        path_elems = FileUtil.path_elems(d)
        if self.settings.excludehidden:
            for p in path_elems:
                if FileUtil.is_hidden(p):
                    return False
        if self.settings.in_dirpatterns and \
                not any_matches_any_pattern(path_elems, self.settings.in_dirpatterns):
            return False
        if self.settings.out_dirpatterns and \
                any_matches_any_pattern(path_elems, self.settings.out_dirpatterns):
            return False
        return True

    def is_matching_stat(self, stat: os.stat_result) -> bool:
        """Check whether the given file stat matches find settings."""
        if (self.settings.minlastmod
            and stat.st_mtime < self.settings.minlastmod.timestamp()) \
                or (self.settings.maxlastmod
                    and stat.st_mtime > self.settings.maxlastmod.timestamp()):
            return False
        if (self.settings.minsize and stat.st_size < self.settings.minsize) \
                or (self.settings.maxsize and stat.st_size > self.settings.maxsize):
            return False
        return True

    def is_matching_archive_file(self, filename: str, stat: os.stat_result) -> bool:
        """Check whether the given archive file matches find settings."""
        ext = FileUtil.get_extension(filename)
        if (self.settings.in_archiveextensions
            and ext not in self.settings.in_archiveextensions) \
            or (self.settings.out_archiveextensions
                and ext in self.settings.out_archiveextensions) \
            or (self.settings.in_archivefilepatterns
                and not matches_any_pattern(filename, self.settings.in_archivefilepatterns)) \
            or (self.settings.out_archivefilepatterns
                and matches_any_pattern(filename, self.settings.out_archivefilepatterns)):
            return False
        return self.is_matching_stat(stat)

    def is_matching_file(self, filename: str, filetype: FileType, stat: os.stat_result) -> bool:
        """Check whether the given file matches find settings."""
        if self.settings.in_extensions or self.settings.out_extensions:
            ext = FileUtil.get_extension(filename)
            if (self.settings.in_extensions and ext not in self.settings.in_extensions) \
                    or (self.settings.out_extensions and ext in self.settings.out_extensions):
                return False
        if (self.settings.in_filepatterns and
                not matches_any_pattern(filename, self.settings.in_filepatterns)) \
                or (self.settings.out_filepatterns and
                    matches_any_pattern(filename, self.settings.out_filepatterns)) \
                or (self.settings.in_filetypes and filetype not in self.settings.in_filetypes) \
                or (self.settings.out_filetypes and filetype in self.settings.out_filetypes):
            return False
        if stat:
            return self.is_matching_stat(stat)
        return True

    def filter_to_file_result(self, filepath: str) -> Optional[FileResult]:
        """Return a FileResult instance if the given filepath matches find settings, else None."""
        (path, filename) = os.path.split(filepath)
        if self.settings.excludehidden and FileUtil.is_hidden(filename):
            return None
        filetype = self.filetypes.get_filetype(filename)
        if filetype == FileType.ARCHIVE \
           and not self.settings.includearchives \
           and not self.settings.archivesonly:
            return None
        stat = None
        if self.settings.need_stat():
            stat = os.stat(filepath)
        if filetype == FileType.ARCHIVE:
            if not self.is_matching_archive_file(filename, stat):
                return None
        elif self.settings.archivesonly or not self.is_matching_file(filename, filetype, stat):
            return None
        return FileResult(path=path, filename=filename, filetype=filetype, stat=stat)

    def find_files(self) -> List[FileResult]:
        """Get the list of all files matching find settings."""
        fileresults = []
        for p in self.settings.paths:
            if os.path.isdir(p):
                if self.is_matching_dir(os.path.abspath(p)):
                    if self.settings.recursive:
                        for root, dirs, files in os.walk(p, topdown=True):
                            # NOTE: skipping self.is_find_dir(root) and checking dirs,
                            #       this has the effect of limiting checks to subdirs
                            #       and removing duplicate checks of settings.paths
                            del_dirs = []
                            for d in dirs:
                                if not self.is_matching_dir(d):
                                    del_dirs.append(d)
                            for d in del_dirs:
                                i = dirs.index(d)
                                del dirs[i]

                            # TODO: add option to follow symlinks? (skipping for now)
                            files = [
                                os.path.join(root, f) for f in files
                                if not os.path.islink(os.path.join(root, f))
                            ]
                            new_fileresults = [self.filter_to_file_result(f) for f in files]
                            fileresults.extend([fr for fr in new_fileresults if fr])
                    else:
                        files = [
                            os.path.join(p, f) for f in os.listdir(p)
                            if os.path.isfile(os.path.join(p, f))
                                and not os.path.islink(os.path.join(p, f))
                        ]
                        new_fileresults = [self.filter_to_file_result(f) for f in files]
                        fileresults.extend([fr for fr in new_fileresults if fr])
            elif os.path.isfile(p):
                fr = self.filter_to_file_result(p)
                if fr:
                    fileresults.append(fr)
        return fileresults

    async def find(self) -> List[FileResult]:
        """Find matching files under paths."""
        return self.sort_file_results(self.find_files())

    def sort_file_results(self, file_results: list[FileResult]) -> list[FileResult]:
        def c(s: str) -> str:
            if self.settings.sort_caseinsensitive:
                return s.lower()
            return s
        match self.settings.sortby:
            case SortBy.FILEPATH:
                return sorted(file_results, key=lambda r: (c(r.path), c(r.filename)),
                              reverse=self.settings.sort_descending)
            case SortBy.FILENAME:
                return sorted(file_results, key=lambda r: (c(r.filename), c(r.path)),
                              reverse=self.settings.sort_descending)
            case SortBy.FILESIZE:
                return sorted(file_results, key=lambda r: (r.stat.st_size, c(r.path), c(r.filename)),
                              reverse=self.settings.sort_descending)
            case SortBy.FILETYPE:
                return sorted(file_results, key=lambda r: (r.filetype, c(r.path), c(r.filename)),
                              reverse=self.settings.sort_descending)
            case SortBy.LASTMOD:
                return sorted(file_results, key=lambda r: (r.stat.st_mtime, c(r.path), c(r.filename)),
                              reverse=self.settings.sort_descending)
            case _:
                return sorted(file_results, key=lambda r: (c(r.path), c(r.filename)),
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
