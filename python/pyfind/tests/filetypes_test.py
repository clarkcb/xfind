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
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)[:-6]))

from pyfind import FileType, FileTypes


class FileTypesTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.file_types = FileTypes()

    def test_get_file_type_archive_file(self):
        file_name = 'archive.zip'
        self.assertEqual(self.file_types.get_file_type(file_name), FileType.ARCHIVE)

    def test_get_file_type_audio_file(self):
        file_name = 'music.mp3'
        self.assertEqual(self.file_types.get_file_type(file_name), FileType.AUDIO)

    def test_get_file_type_binary_file(self):
        file_name = 'binary.exe'
        self.assertEqual(self.file_types.get_file_type(file_name), FileType.BINARY)

    def test_get_file_type_code_file(self):
        file_name = 'code.py'
        self.assertEqual(self.file_types.get_file_type(file_name), FileType.CODE)

    def test_get_file_type_font_file(self):
        file_name = 'font.ttf'
        self.assertEqual(self.file_types.get_file_type(file_name), FileType.FONT)

    def test_get_file_type_image_file(self):
        file_name = 'image.png'
        self.assertEqual(self.file_types.get_file_type(file_name), FileType.IMAGE)

    def test_get_file_type_text_file(self):
        file_name = 'text.txt'
        self.assertEqual(self.file_types.get_file_type(file_name), FileType.TEXT)

    def test_get_file_type_video_file(self):
        file_name = 'movie.mp4'
        self.assertEqual(self.file_types.get_file_type(file_name), FileType.VIDEO)

    def test_get_file_type_unknown_file(self):
        file_name = 'unknown.xyz'
        self.assertEqual(self.file_types.get_file_type(file_name), FileType.UNKNOWN)

    def test_is_archive_file(self):
        file_name = 'archive.tar.bz2'
        self.assertTrue(self.file_types.is_archive_file(file_name))

    def test_is_audio_file(self):
        file_name = 'audio.mid'
        self.assertTrue(self.file_types.is_audio_file(file_name))

    def test_is_binary_file(self):
        file_name = 'binary.dylib'
        self.assertTrue(self.file_types.is_binary_file(file_name))

    def test_is_code_file(self):
        file_name = 'code.py'
        self.assertTrue(self.file_types.is_code_file(file_name))

    def test_is_font_file(self):
        file_name = 'typeset.otf'
        self.assertTrue(self.file_types.is_font_file(file_name))

    def test_is_image_file(self):
        file_name = 'xml-image.svg'
        self.assertTrue(self.file_types.is_image_file(file_name))

    def test_is_video_file(self):
        file_name = 'vid.mov'
        self.assertTrue(self.file_types.is_video_file(file_name))

    def test_is_xml_file(self):
        file_name = 'file.xml'
        self.assertTrue(self.file_types.is_xml_file(file_name))

    def test_is_unknown_file(self):
        file_name = 'unknown.xyz'
        self.assertTrue(self.file_types.is_unknown_file(file_name))


if __name__ == '__main__':
    unittest.main()
