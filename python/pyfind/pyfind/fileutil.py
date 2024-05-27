# -*- coding: utf-8 -*-
"""
###############################################################################
#
# fileutil.py
#
# class FileUtil: provides utility functions for getting file extension and
#                 checking for hidden files and directories
#
###############################################################################
"""
import os
from pathlib import Path


class FileUtil:
    """a file helper class"""

    DOT_DIRS = frozenset(['.', '..'])
    DOT_DIR_PATHS = frozenset([Path(d) for d in DOT_DIRS])

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
    def get_path_extension(file_path: Path) -> str:
        """Returns the extension for a given file_name, if any, else empty
           string"""
        ext = file_path.suffix
        if ext and ext[0] == '.':  # remove leading dot
            ext = ext[1:]
        if ext == 'Z':
            return ext
        return ext.lower()

    @staticmethod
    def is_dot_dir(file_path: str) -> bool:
        """Returns true if file_path is dot dir (. or ..)"""
        if not file_path:
            return False
        return Path(file_path) in FileUtil.DOT_DIR_PATHS

    @staticmethod
    def is_dot_dir_path(file_path: Path) -> bool:
        """Returns true if file_path is dot dir (. or ..)"""
        if not file_path:
            return False
        return file_path in FileUtil.DOT_DIR_PATHS

    @staticmethod
    def is_hidden(file_path: str) -> bool:
        """Returns true if any part of file_path is hidden else false"""
        if FileUtil.is_dot_dir(file_path):
            return False
        return any(
            p.startswith('.') and not FileUtil.is_dot_dir(p)
            for p in file_path.split(os.path.sep)
        )

    @staticmethod
    def is_hidden_path(file_path: Path) -> bool:
        """Returns true if any part of file_path is hidden else false"""
        if FileUtil.is_dot_dir_path(file_path):
            return False
        return any(p.startswith('.') and not FileUtil.is_dot_dir(p) for p in file_path.parts)
