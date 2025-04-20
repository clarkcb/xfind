# -*- coding: utf-8 -*-
"""
################################################################################
#
# fileresult_test.py
#
# class FileResultTest: testing of FileResult
#
################################################################################
"""
import os
from pathlib import Path
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)[:-6]))

from pyfind import FileType, FileResult, XFIND_PATH


class FileResultTest(unittest.TestCase):

    def test_file_result_abs_path(self):
        file_name = 'fileresult.py'
        path = Path(XFIND_PATH, 'python/pyfind/pyfind', file_name)
        file_result = FileResult(path=path, file_type=FileType.CODE)
        self.assertEqual(path, file_result.path)
        self.assertEqual(file_name, file_result.path.name)
        self.assertEqual(FileType.CODE, file_result.file_type)

    def test_file_result_rel_path1(self):
        file_name = 'fileresult.py'
        path = Path('.', file_name)
        file_result = FileResult(path=path, file_type=FileType.CODE)
        self.assertEqual(path, file_result.path)
        self.assertEqual(file_name, file_result.path.name)
        self.assertEqual(FileType.CODE, file_result.file_type)

    def test_file_result_rel_path2(self):
        file_name = 'fileresult.py'
        path = Path('./', file_name)
        file_result = FileResult(path=path, file_type=FileType.CODE)
        self.assertEqual(path, file_result.path)
        self.assertEqual(file_name, file_result.path.name)
        self.assertEqual(FileType.CODE, file_result.file_type)

    def test_file_result_rel_path3(self):
        file_name = 'fileresult.py'
        path = Path('..', file_name)
        file_result = FileResult(path=path, file_type=FileType.CODE)
        self.assertEqual(path, file_result.path)
        self.assertEqual(file_name, file_result.path.name)
        self.assertEqual(FileType.CODE, file_result.file_type)


if __name__ == '__main__':
    unittest.main()
