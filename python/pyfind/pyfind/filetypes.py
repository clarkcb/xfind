# -*- coding: utf-8 -*-
"""
###############################################################################
#
# filetypes.py
#
# class FileTypes
#
###############################################################################
"""
import json
from enum import Enum

import pkg_resources

from .fileutil import FileUtil
from .findexception import FindException


class FileType(Enum):
    """FileType enum"""
    UNKNOWN = 0
    ARCHIVE = 1
    BINARY = 2
    CODE = 3
    TEXT = 4
    XML = 5

    @classmethod
    def from_name(cls, name):
        """Return FileType for given name if found else raise FindException"""
        uname = name.upper()
        try:
            return FileType[uname]
        except KeyError:
            raise FindException(f'Invalid file type: {name}\n')


class FileTypes:
    """a class to provide file type information"""

    TEXT_TYPES = frozenset([FileType.CODE, FileType.TEXT, FileType.XML])

    __slots__ = ['__filetype_exts', '__filetype_names']

    def __init__(self):
        self.__filetype_exts = {}
        self.__filetype_names = {}
        self.__populate_filetypes_from_json()

    def get_filetype(self, filename: str) -> FileType:
        """Return file type for filename"""
        if self.is_code_file(filename):
            return FileType.CODE
        if self.is_xml_file(filename):
            return FileType.XML
        if self.is_text_file(filename):
            return FileType.TEXT
        if self.is_binary_file(filename):
            return FileType.BINARY
        if self.is_archive_file(filename):
            return FileType.ARCHIVE
        return FileType.UNKNOWN

    def is_archive_file(self, f: str) -> bool:
        """Return true if file is of a (known) archive file type"""
        return f in self.__filetype_names['archive'] or \
               FileUtil.get_extension(f) in self.__filetype_exts['archive']

    def is_binary_file(self, f: str) -> bool:
        """Return true if file is of a (known) findable binary file type"""
        return f in self.__filetype_names['binary'] or \
               FileUtil.get_extension(f) in self.__filetype_exts['binary']

    def is_code_file(self, f: str) -> bool:
        """Return true if file is of a (known) code file type"""
        return f in self.__filetype_names['code'] or \
               FileUtil.get_extension(f) in self.__filetype_exts['code']

    def is_text_file(self, f: str) -> bool:
        """Return true if file is of a (known) text file type"""
        return f in self.__filetype_names['text'] or \
               FileUtil.get_extension(f) in self.__filetype_exts['text']

    def is_xml_file(self, f: str) -> bool:
        """Return true if file is of a (known) xml file type"""
        return f in self.__filetype_names['xml'] or \
               FileUtil.get_extension(f) in self.__filetype_exts['xml']

    def is_unknown_file(self, f: str) -> bool:
        """Return true if file is of an unknown file type"""
        return self.get_filetype(f) == FileType.UNKNOWN

    def __populate_filetypes_from_json(self):
        filetypes_dict = {}
        with pkg_resources.resource_stream(__name__, 'data/filetypes.json') as stream:
            filetypes_dict = json.load(stream)
        for filetype_obj in filetypes_dict['filetypes']:
            typename = filetype_obj['type']
            self.__filetype_exts[typename] = set(filetype_obj['extensions'])
            if 'names' in filetype_obj:
                self.__filetype_names[typename] = set(filetype_obj['names'])
            else:
                self.__filetype_names[typename] = set([])
        self.__filetype_exts['text'].update(self.__filetype_exts['code'],
                                            self.__filetype_exts['xml'])
        self.__filetype_names['text'].update(self.__filetype_names['code'],
                                             self.__filetype_names['xml'])
        self.__filetype_exts['findable'] = \
            self.__filetype_exts['binary'].union(self.__filetype_exts['archive'],
                                                 self.__filetype_exts['text'])
        self.__filetype_names['findable'] = \
            self.__filetype_names['binary'].union(self.__filetype_names['archive'],
                                                  self.__filetype_names['text'])
