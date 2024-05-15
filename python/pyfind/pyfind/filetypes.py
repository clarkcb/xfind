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
from enum import StrEnum

from .fileutil import FileUtil


class FileType(StrEnum):
    """FileType enum"""
    UNKNOWN = 'unknown'
    ARCHIVE = 'archive'
    AUDIO = 'audio'
    BINARY = 'binary'
    CODE = 'code'
    FONT = 'font'
    IMAGE = 'image'
    TEXT = 'text'
    VIDEO = 'video'
    XML = 'xml'

    def __lt__(self, other):
        if self.__class__ is other.__class__:
            return self.value < other.value
        return NotImplemented

    @classmethod
    def from_name(cls, name):
        """Return FileType for given name if found else return UNKNOWN"""
        try:
            return FileType[name.strip().upper()]
        except KeyError:
            return FileType.UNKNOWN


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
        # more specific first
        if self.is_code_file(file_name):
            return FileType.CODE
        if self.is_archive_file(file_name):
            return FileType.ARCHIVE
        if self.is_audio_file(file_name):
            return FileType.AUDIO
        if self.is_font_file(file_name):
            return FileType.FONT
        if self.is_image_file(file_name):
            return FileType.IMAGE
        if self.is_video_file(file_name):
            return FileType.VIDEO
        # more general last
        if self.is_xml_file(file_name):
            return FileType.XML
        if self.is_text_file(file_name):
            return FileType.TEXT
        if self.is_binary_file(file_name):
            return FileType.BINARY
        return FileType.UNKNOWN

    def is_archive_file(self, f: str) -> bool:
        """Return true if file is of a (known) archive file type"""
        return f in self.__file_type_names[FileType.ARCHIVE] or \
               FileUtil.get_extension(f) in self.__file_type_exts[FileType.ARCHIVE]

    def is_audio_file(self, f: str) -> bool:
        """Return true if file is of a (known) audio file type"""
        return f in self.__file_type_names[FileType.AUDIO] or \
               FileUtil.get_extension(f) in self.__file_type_exts[FileType.AUDIO]

    def is_binary_file(self, f: str) -> bool:
        """Return true if file is of a (known) findable binary file type"""
        return f in self.__file_type_names[FileType.BINARY] or \
               FileUtil.get_extension(f) in self.__file_type_exts[FileType.BINARY]

    def is_code_file(self, f: str) -> bool:
        """Return true if file is of a (known) code file type"""
        return f in self.__file_type_names[FileType.CODE] or \
               FileUtil.get_extension(f) in self.__file_type_exts[FileType.CODE]

    def is_font_file(self, f: str) -> bool:
        """Return true if file is of a (known) font file type"""
        return f in self.__file_type_names[FileType.FONT] or \
               FileUtil.get_extension(f) in self.__file_type_exts[FileType.FONT]

    def is_image_file(self, f: str) -> bool:
        """Return true if file is of a (known) image file type"""
        return f in self.__file_type_names[FileType.IMAGE] or \
               FileUtil.get_extension(f) in self.__file_type_exts[FileType.IMAGE]

    def is_text_file(self, f: str) -> bool:
        """Return true if file is of a (known) text file type"""
        return f in self.__file_type_names[FileType.TEXT] or \
               FileUtil.get_extension(f) in self.__file_type_exts[FileType.TEXT]

    def is_video_file(self, f: str) -> bool:
        """Return true if file is of a (known) video file type"""
        return f in self.__file_type_names[FileType.VIDEO] or \
               FileUtil.get_extension(f) in self.__file_type_exts[FileType.VIDEO]

    def is_xml_file(self, f: str) -> bool:
        """Return true if file is of a (known) xml file type"""
        return f in self.__file_type_names[FileType.XML] or \
               FileUtil.get_extension(f) in self.__file_type_exts[FileType.XML]

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
                                                  self.__file_type_exts['audio'],
                                                  self.__file_type_exts['font'],
                                                  self.__file_type_exts['image'],
                                                  self.__file_type_exts['text'],
                                                  self.__file_type_exts['video'])
        self.__file_type_names['findable'] = \
            self.__file_type_names['binary'].union(self.__file_type_names['archive'],
                                                   self.__file_type_names['audio'],
                                                   self.__file_type_names['font'],
                                                   self.__file_type_names['image'],
                                                   self.__file_type_names['text'],
                                                   self.__file_type_names['video'])
