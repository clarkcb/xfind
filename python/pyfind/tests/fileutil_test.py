# -*- coding: utf-8 -*-
"""
################################################################################
#
# fileutil_test.py
#
# class FileUtilTest: testing of FileUtil
#
################################################################################
"""
import os
from pathlib import Path
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)[:-6]))

from pyfind import FileUtil


class FileUtilTest(unittest.TestCase):
    ############################################################################
    # get_extension tests
    ############################################################################
    def test_get_extension_has_txt_extension(self):
        file_name = 'filename.txt'
        expected_ext = 'txt'
        actual_ext = FileUtil.get_extension(file_name)
        self.assertEqual(expected_ext, actual_ext)

    def test_get_extension_missing_extension(self):
        file_name = 'filename.'
        self.assertEqual(FileUtil.get_extension(file_name), '')

    def test_get_extension_no_extension(self):
        file_name = 'filename'
        self.assertEqual(FileUtil.get_extension(file_name), '')

    def test_get_extension_hidden_txt_file(self):
        file_name = '.hidden.txt'
        self.assertEqual(FileUtil.get_extension(file_name), 'txt')

    def test_get_extension_hidden_file_missing_extension(self):
        file_name = '.hidden.'
        self.assertEqual(FileUtil.get_extension(file_name), '')

    def test_get_extension_hidden_file_no_extension(self):
        file_name = '.hidden'
        self.assertEqual(FileUtil.get_extension(file_name), '')

    ############################################################################
    # get_path_extension tests
    ############################################################################
    def test_get_path_extension_has_txt_extension(self):
        file_path = Path('filename.txt')
        expected_ext = 'txt'
        actual_ext = FileUtil.get_path_extension(file_path)
        self.assertEqual(expected_ext, actual_ext)

    def test_get_path_extension_missing_extension(self):
        file_path = Path('filename.')
        self.assertEqual(FileUtil.get_path_extension(file_path), '')

    def test_get_path_extension_no_extension(self):
        file_path = Path('filename')
        self.assertEqual(FileUtil.get_path_extension(file_path), '')

    def test_get_path_extension_hidden_txt_file(self):
        file_path = Path('.hidden.txt')
        self.assertEqual(FileUtil.get_path_extension(file_path), 'txt')

    def test_get_path_extension_hidden_file_missing_extension(self):
        file_path = Path('.hidden.')
        self.assertEqual(FileUtil.get_path_extension(file_path), '')

    def test_get_path_extension_hidden_file_no_extension(self):
        file_path = Path('.hidden')
        self.assertEqual(FileUtil.get_path_extension(file_path), '')

    ############################################################################
    # is_dot_dir tests
    ############################################################################
    def test_is_dot_dir_single_dot(self):
        file_name = '.'
        self.assertTrue(FileUtil.is_dot_dir(file_name))

    def test_is_dot_dir_double_dot(self):
        file_name = '..'
        self.assertTrue(FileUtil.is_dot_dir(file_name))

    def test_is_dot_dir_non_dot_dir(self):
        file_name = '.git'
        self.assertFalse(FileUtil.is_dot_dir(file_name))

    ############################################################################
    # is_dot_dir_path tests
    ############################################################################
    def test_is_dot_dir_path_single_dot(self):
        file_path = Path('.')
        self.assertTrue(FileUtil.is_dot_dir_path(file_path))

    def test_is_dot_dir_path_double_dot(self):
        file_path = Path('..')
        self.assertTrue(FileUtil.is_dot_dir_path(file_path))

    def test_is_dot_dir_path_non_dot_dir(self):
        file_path = Path('.git')
        self.assertFalse(FileUtil.is_dot_dir_path(file_path))

    ############################################################################
    # is_hidden tests
    ############################################################################
    def test_is_hidden_hidden_file(self):
        file_name = '.filename.txt'
        self.assertTrue(FileUtil.is_hidden(file_name))

    def test_is_hidden_not_hidden_file(self):
        file_name = 'filename.txt'
        self.assertFalse(FileUtil.is_hidden(file_name))

    def test_is_hidden_single_dot(self):
        file_name = '.'
        self.assertFalse(FileUtil.is_hidden(file_name))

    def test_is_hidden_double_dot(self):
        file_name = '..'
        self.assertFalse(FileUtil.is_hidden(file_name))

    ############################################################################
    # is_hidden_path tests
    ############################################################################
    def test_is_hidden_path_hidden_file(self):
        file_path = Path('.filename.txt')
        self.assertTrue(FileUtil.is_hidden_path(file_path))

    def test_is_hidden_path_not_hidden_file(self):
        file_path = Path('filename.txt')
        self.assertFalse(FileUtil.is_hidden_path(file_path))

    def test_is_hidden_path_single_dot(self):
        file_path = Path('.')
        self.assertFalse(FileUtil.is_hidden_path(file_path))

    def test_is_hidden_path_double_dot(self):
        file_path = Path('..')
        self.assertFalse(FileUtil.is_hidden_path(file_path))


if __name__ == '__main__':
    unittest.main()
