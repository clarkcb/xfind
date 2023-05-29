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
import importlib.resources
import json
from enum import Enum

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

    def __lt__(self, other):
        if self.__class__ is other.__class__:
            return self.value < other.value
        return NotImplemented

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

    __slots__ = ['__file_type_exts', '__file_type_names']

    def __init__(self):
        self.__file_type_exts = {}
        self.__file_type_names = {}
        self.__populate_file_types_from_json()

    def get_file_type(self, file_name: str) -> FileType:
        """Return file type for file_name"""
        if self.is_code_file(file_name):
            return FileType.CODE
        if self.is_xml_file(file_name):
            return FileType.XML
        if self.is_text_file(file_name):
            return FileType.TEXT
        if self.is_binary_file(file_name):
            return FileType.BINARY
        if self.is_archive_file(file_name):
            return FileType.ARCHIVE
        return FileType.UNKNOWN

    def is_archive_file(self, f: str) -> bool:
        """Return true if file is of a (known) archive file type"""
        return f in self.__file_type_names['archive'] or \
               FileUtil.get_extension(f) in self.__file_type_exts['archive']

    def is_binary_file(self, f: str) -> bool:
        """Return true if file is of a (known) findable binary file type"""
        return f in self.__file_type_names['binary'] or \
               FileUtil.get_extension(f) in self.__file_type_exts['binary']

    def is_code_file(self, f: str) -> bool:
        """Return true if file is of a (known) code file type"""
        return f in self.__file_type_names['code'] or \
               FileUtil.get_extension(f) in self.__file_type_exts['code']

    def is_text_file(self, f: str) -> bool:
        """Return true if file is of a (known) text file type"""
        return f in self.__file_type_names['text'] or \
               FileUtil.get_extension(f) in self.__file_type_exts['text']

    def is_xml_file(self, f: str) -> bool:
        """Return true if file is of a (known) xml file type"""
        return f in self.__file_type_names['xml'] or \
               FileUtil.get_extension(f) in self.__file_type_exts['xml']

    def is_unknown_file(self, f: str) -> bool:
        """Return true if file is of an unknown file type"""
        return self.get_file_type(f) == FileType.UNKNOWN

    def __populate_file_types_from_json(self):
        data = importlib.resources.files('pyfind').joinpath('data')
        file_types_json = data.joinpath('filetypes.json').read_text()
        file_types_dict = json.loads(file_types_json)
        for file_type_obj in file_types_dict['filetypes']:
            typename = file_type_obj['type']
            self.__file_type_exts[typename] = set(file_type_obj['extensions'])
            if 'names' in file_type_obj:
                self.__file_type_names[typename] = set(file_type_obj['names'])
            else:
                self.__file_type_names[typename] = set([])
        self.__file_type_exts['text'].update(self.__file_type_exts['code'],
                                             self.__file_type_exts['xml'])
        self.__file_type_names['text'].update(self.__file_type_names['code'],
                                              self.__file_type_names['xml'])
        self.__file_type_exts['findable'] = \
            self.__file_type_exts['binary'].union(self.__file_type_exts['archive'],
                                                  self.__file_type_exts['text'])
        self.__file_type_names['findable'] = \
            self.__file_type_names['binary'].union(self.__file_type_names['archive'],
                                                   self.__file_type_names['text'])
