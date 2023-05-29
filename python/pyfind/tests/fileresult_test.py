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
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)[:-6]))

from pyfind import FileType, FileResult, XFINDPATH


class FileResultTest(unittest.TestCase):

    def test_file_result_abs_path(self):
        path = XFINDPATH + '/python/pyfind/pyfind'
        file_name = 'fileresult.py'
        file_result = FileResult(path=path, file_name=file_name, file_type=FileType.CODE)
        self.assertEqual(XFINDPATH + '/python/pyfind/pyfind/fileresult.py', file_result.relative_path)
        self.assertEqual(XFINDPATH + '/python/pyfind/pyfind/fileresult.py', str(file_result))

    def test_file_result_rel_path1(self):
        path = '.'
        file_name = 'fileresult.py'
        file_result = FileResult(path=path, file_name=file_name, file_type=FileType.CODE)
        self.assertEqual('./fileresult.py', file_result.relative_path)
        self.assertEqual('./fileresult.py', str(file_result))

    def test_file_result_rel_path2(self):
        path = './'
        file_name = 'fileresult.py'
        file_result = FileResult(path=path, file_name=file_name, file_type=FileType.CODE)
        self.assertEqual('./fileresult.py', file_result.relative_path)
        self.assertEqual('./fileresult.py', str(file_result))

    def test_file_result_rel_path3(self):
        path = '..'
        file_name = 'fileresult.py'
        file_result = FileResult(path=path, file_name=file_name, file_type=FileType.CODE)
        self.assertEqual('../fileresult.py', file_result.relative_path)
        self.assertEqual('../fileresult.py', str(file_result))


if __name__ == '__main__':
    unittest.main()
