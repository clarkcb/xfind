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
from enum import Enum
from pathlib import Path
import sqlite3

from .config import XFINDDB
from .fileutil import FileUtil


class FileType(Enum):
    """FileType enum"""
    UNKNOWN = 1
    ARCHIVE = 2
    AUDIO = 3
    BINARY = 4
    CODE = 5
    FONT = 6
    IMAGE = 7
    TEXT = 8
    VIDEO = 9
    XML = 10

    @classmethod
    def from_name(cls, name: str):
        """Return FileType for given name if found else return UNKNOWN"""
        uname = name.upper()
        fts = [_ft for _ft in FileType if _ft.name == uname]
        if fts:
            return fts[0]
        return FileType.UNKNOWN

    def __lt__(self, other):
        if self.__class__ is other.__class__:
            return self.value < other.value
        return NotImplemented

    def __str__(self):
        return self.name.lower()


class FileTypes:
    """a class to provide file type information"""

    TEXT_TYPES = frozenset([FileType.CODE, FileType.TEXT, FileType.XML])

    ZIPFILE_EXTENSIONS = frozenset(['zip', 'jar', 'war', 'ear', 'obr', 'apk', 'xpi', 'xap', 'xar', 'epub'])
    TARFILE_EXTENSIONS = frozenset(['tar', 'tgz', 'bz2', 'gz', 'xz', 'Z', 'lz', 'lzma'])

    __slots__ = ['__db_connection', '__db_cursor', '__file_types', '__ext_file_type_cache', '__name_file_type_cache']

    def __init__(self):
        self.__db_connection = None
        self.__db_cursor = None
        self.__file_types = list(FileType)
        self.__ext_file_type_cache = {}
        self.__name_file_type_cache = {}
        self.__load_name_cache()

    def __load_name_cache(self):
        """Load name cache"""
        query = 'SELECT name, file_type_id FROM file_name'
        res = self.__query_all(query)
        for r in res:
            self.__name_file_type_cache[r[0]] = self.__file_types[r[1] - 1]

    def __query_all(self, query: str, params=None):
        """Return multiple results from query"""
        if not self.__db_cursor:
            self.__db_cursor = self.__get_cursor()
        if not params:
            params = []
        return self.__db_cursor.execute(query, params).fetchall()

    def __query_one(self, query: str, params=None):
        """Return one result from query"""
        if not self.__db_cursor:
            self.__db_cursor = self.__get_cursor()
        if not params:
            params = []
        return self.__db_cursor.execute(query, params).fetchone()

    def get_file_type_for_name(self, name: str) -> FileType:
        """Return FileType for given file name"""
        if name in self.__name_file_type_cache:
            return self.__name_file_type_cache[name]
        # Since the cache is loaded with all db file names, this is not needed
        # query = 'SELECT file_type_id FROM file_name WHERE name = ?'
        # params = (name,)
        # res = self.__query_one(query, params)
        # if res:
        #     file_type = self.__file_types[res[0] - 1]
        #     self.__name_file_type_cache[name] = file_type
        #     return file_type
        return FileType.UNKNOWN

    def get_file_type_for_extension(self, ext: str) -> FileType:
        """Return FileType for given file extension"""
        if ext in self.__ext_file_type_cache:
            return self.__ext_file_type_cache[ext]
        query = 'SELECT file_type_id FROM file_extension WHERE extension = ?'
        params = (ext,)
        res = self.__query_one(query, params)
        if res:
            file_type = self.__file_types[res[0] - 1]
            self.__ext_file_type_cache[ext] = file_type
            return file_type
        return FileType.UNKNOWN

    def get_file_type_for_path(self, file_path: Path) -> FileType:
        """Return file type for file_path"""
        file_type_for_file_name = self.get_file_type_for_name(file_path.name)
        if file_type_for_file_name != FileType.UNKNOWN:
            return file_type_for_file_name
        return self.get_file_type_for_extension(FileUtil.get_path_extension(file_path))

    def is_archive_file_path(self, p: Path) -> bool:
        """Return true if file is of a (known) archive file type"""
        return self.get_file_type_for_path(p) == FileType.ARCHIVE

    def is_audio_file_path(self, p: Path) -> bool:
        """Return true if file is of a (known) audio file type"""
        return self.get_file_type_for_path(p) == FileType.AUDIO

    def is_binary_file_path(self, p: Path) -> bool:
        """Return true if file is of a (known) findable binary file type"""
        return self.get_file_type_for_path(p) == FileType.BINARY

    def is_code_file_path(self, p: Path) -> bool:
        """Return true if file is of a (known) code file type"""
        return self.get_file_type_for_path(p) == FileType.CODE

    def is_font_file_path(self, p: Path) -> bool:
        """Return true if file is of a (known) font file type"""
        return self.get_file_type_for_path(p) == FileType.FONT

    def is_image_file_path(self, p: Path) -> bool:
        """Return true if file is of a (known) image file type"""
        return self.get_file_type_for_path(p) == FileType.IMAGE

    def is_text_file_path(self, p: Path) -> bool:
        """Return true if file is of a (known) text file type"""
        file_type = self.get_file_type_for_path(p)
        return file_type == FileType.TEXT or file_type == FileType.CODE or file_type == FileType.XML

    def is_video_file_path(self, p: Path) -> bool:
        """Return true if file is of a (known) video file type"""
        return self.get_file_type_for_path(p) == FileType.VIDEO

    def is_xml_file_path(self, p: Path) -> bool:
        """Return true if file is of a (known) xml file type"""
        return self.get_file_type_for_path(p) == FileType.XML

    def is_unknown_file_path(self, p: Path) -> bool:
        """Return true if file is of an unknown file type"""
        return self.get_file_type_for_path(p) == FileType.UNKNOWN

    def __get_cursor(self):
        """Return a connection to the database"""
        if not self.__db_connection:
            self.__db_connection = sqlite3.connect(f'file:{XFINDDB}?mode=ro', uri=True)
        return self.__db_connection.cursor()
