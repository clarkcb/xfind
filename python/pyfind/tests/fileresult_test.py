# -*- coding: utf-8 -*-
################################################################################
#
# fileresult_test.py
#
# class FileResultTest: testing of FileResult
#
################################################################################
import os
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from pyfind import FileType, FileResult, XFINDPATH


class FileResultTest(unittest.TestCase):

    def test_fileresult_abs_path(self):
        path = XFINDPATH + '/python/pyfind/pyfind'
        filename = 'fileresult.py'
        fileresult = FileResult(path=path, filename=filename, filetype=FileType.CODE)
        self.assertEqual(XFINDPATH + '/python/pyfind/pyfind/fileresult.py', fileresult.relativepath)
        self.assertEqual(XFINDPATH + '/python/pyfind/pyfind/fileresult.py', str(fileresult))

    def test_fileresult_rel_path1(self):
        path = '.'
        filename = 'fileresult.py'
        fileresult = FileResult(path=path, filename=filename, filetype=FileType.CODE)
        self.assertEqual('./fileresult.py', fileresult.relativepath)
        self.assertEqual('./fileresult.py', str(fileresult))

    def test_fileresult_rel_path2(self):
        path = './'
        filename = 'fileresult.py'
        fileresult = FileResult(path=path, filename=filename, filetype=FileType.CODE)
        self.assertEqual('./fileresult.py', fileresult.relativepath)
        self.assertEqual('./fileresult.py', str(fileresult))

    def test_fileresult_rel_path3(self):
        path = '..'
        filename = 'fileresult.py'
        fileresult = FileResult(path=path, filename=filename, filetype=FileType.CODE)
        self.assertEqual('../fileresult.py', fileresult.relativepath)
        self.assertEqual('../fileresult.py', str(fileresult))


if __name__ == '__main__':
    unittest.main()
