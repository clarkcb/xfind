# -*- coding: utf-8 -*-
"""
###############################################################################
#
# fileutil.py
#
# class FileUtil: provides utility functions for getting file extension and
#                 determining file type
#
###############################################################################
"""
import os


class FileUtil:
    """a file helper class"""

    DOT_DIRS = frozenset(['.', '..'])

    @staticmethod
    def expand_path(file_path: str) -> str:
        """Returns expanded version of path: ~ substitution and absolute path"""
        expanded = file_path
        if expanded.startswith('~'):
            expanded = FileUtil.get_home() + expanded[1:]
        return expanded

    @staticmethod
    def get_extension(file_name: str) -> str:
        """Returns the extension for a given file_name, if any, else empty
           string"""
        ext = ''
        if os.path.basename(file_name).rfind('.') > 0:
            ext = file_name.rpartition('.')[2]
            if ext == 'Z':
                return ext
        return ext.lower()

    @staticmethod
    def get_home() -> str:
        """Returns user's home path, e.g. /home/cary"""
        home = os.getenv('HOME')
        if not home:
            home = os.getenv('USERPROFILE')
        return home

    @staticmethod
    def is_dot_dir(file_path: str) -> bool:
        """Returns true if file_path is dot dir (. or ..)"""
        if not file_path:
            return False
        return file_path in FileUtil.DOT_DIRS or \
            (file_path.endswith('/') and file_path[:-1] in FileUtil.DOT_DIRS)

    @staticmethod
    def is_hidden(file_path: str) -> bool:
        """Returns true if file_path is hidden else false"""
        if FileUtil.is_dot_dir(file_path):
            return False
        dot_elems = [
            e for e in file_path.split(os.path.sep)
            if e.startswith('.') and not FileUtil.is_dot_dir(e)
        ]
        return len(dot_elems) > 0

    @staticmethod
    def path_depth(file_path: str) -> int:
        """Returns the number of path elements of file_path"""
        return len([c for c in file_path if c == os.sep])
        # return [p for p in file_path.split(os.sep) if p and p not in FileUtil.DOT_DIRS]

    @staticmethod
    def path_elems(file_path: str) -> list[str]:
        """Returns list of separate path elements of file_path"""
        return [p for p in file_path.split(os.sep) if p and p not in FileUtil.DOT_DIRS]

    @staticmethod
    def sep_count(file_path: str) -> int:
        """Returns the number of path separators in file_path"""
        return len([c for c in file_path if c == os.sep])
