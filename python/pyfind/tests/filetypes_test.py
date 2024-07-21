# -*- coding: utf-8 -*-
"""
################################################################################
#
# filetypes_test.py
#
# class FileTypesTest: testing of FileTypes
#
################################################################################
"""
import os
from pathlib import Path
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)[:-6]))

from pyfind import FileType, FileTypes


class FileTypesTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.file_types = FileTypes()

    def test_get_file_type_archive_file(self):
        file_path = Path('archive.zip')
        self.assertEqual(self.file_types.get_file_type_for_path(file_path), FileType.ARCHIVE)

    def test_get_file_type_audio_file(self):
        file_path = Path('music.mp3')
        self.assertEqual(self.file_types.get_file_type_for_path(file_path), FileType.AUDIO)

    def test_get_file_type_binary_file(self):
        file_path = Path('binary.exe')
        self.assertEqual(self.file_types.get_file_type_for_path(file_path), FileType.BINARY)

    def test_get_file_type_code_file(self):
        file_path = Path('code.py')
        self.assertEqual(self.file_types.get_file_type_for_path(file_path), FileType.CODE)

    def test_get_file_type_font_file(self):
        file_path = Path('font.ttf')
        self.assertEqual(self.file_types.get_file_type_for_path(file_path), FileType.FONT)

    def test_get_file_type_image_file(self):
        file_path = Path('image.png')
        self.assertEqual(self.file_types.get_file_type_for_path(file_path), FileType.IMAGE)

    def test_get_file_type_text_file(self):
        file_path = Path('text.txt')
        self.assertEqual(self.file_types.get_file_type_for_path(file_path), FileType.TEXT)

    def test_get_file_type_video_file(self):
        file_path = Path('movie.mp4')
        self.assertEqual(self.file_types.get_file_type_for_path(file_path), FileType.VIDEO)

    def test_get_file_type_unknown_file(self):
        file_path = Path('unknown.xyz')
        self.assertEqual(self.file_types.get_file_type_for_path(file_path), FileType.UNKNOWN)

    def test_is_archive_file(self):
        file_path = Path('archive.tar.bz2')
        self.assertTrue(self.file_types.is_archive_file_path(file_path))

    def test_is_audio_file(self):
        file_path = Path('audio.mid')
        self.assertTrue(self.file_types.is_audio_file_path(file_path))

    def test_is_binary_file(self):
        file_path = Path('binary.dylib')
        self.assertTrue(self.file_types.is_binary_file_path(file_path))

    def test_is_code_file(self):
        file_path = Path('code.py')
        self.assertTrue(self.file_types.is_code_file_path(file_path))

    def test_is_code_file_by_name(self):
        file_path = Path('CMakeLists.txt')
        self.assertTrue(self.file_types.is_code_file_path(file_path))

    def test_is_font_file(self):
        file_path = Path('typeset.otf')
        self.assertTrue(self.file_types.is_font_file_path(file_path))

    def test_is_image_file(self):
        file_path = Path('xml-image.svg')
        self.assertTrue(self.file_types.is_image_file_path(file_path))

    def test_is_video_file(self):
        file_path = Path('vid.mov')
        self.assertTrue(self.file_types.is_video_file_path(file_path))

    def test_is_xml_file(self):
        file_path = Path('file.xml')
        self.assertTrue(self.file_types.is_xml_file_path(file_path))

    def test_is_unknown_file(self):
        file_path = Path('unknown.xyz')
        self.assertTrue(self.file_types.is_unknown_file_path(file_path))


if __name__ == '__main__':
    unittest.main()
