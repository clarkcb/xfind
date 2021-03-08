# -*- coding: utf-8 -*-
################################################################################
#
# findfile_test.py
#
# class FindFileTest: testing of FindFile
#
################################################################################
import os
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from pyfind import FileType, FindFile


class FindFileTest(unittest.TestCase):

    def test_findfile_abs_path(self):
        path = os.environ['HOME'] + '/src/xfind/python/pyfind'
        filename = 'findfile.py'
        findfile = FindFile(path=path, filename=filename, filetype=FileType.CODE)
        self.assertEqual(os.environ['HOME'] + '/src/xfind/python/pyfind/findfile.py', findfile.relativepath)
        self.assertEqual(os.environ['HOME'] + '/src/xfind/python/pyfind/findfile.py', str(findfile))

    def test_findfile_rel_path1(self):
        path = '.'
        filename = 'findfile.py'
        findfile = FindFile(path=path, filename=filename, filetype=FileType.CODE)
        self.assertEqual('./findfile.py', findfile.relativepath)
        self.assertEqual('./findfile.py', str(findfile))

    def test_findfile_rel_path2(self):
        path = './'
        filename = 'findfile.py'
        findfile = FindFile(path=path, filename=filename, filetype=FileType.CODE)
        self.assertEqual('./findfile.py', findfile.relativepath)
        self.assertEqual('./findfile.py', str(findfile))

    def test_findfile_rel_path3(self):
        path = '..'
        filename = 'findfile.py'
        findfile = FindFile(path=path, filename=filename, filetype=FileType.CODE)
        self.assertEqual('../findfile.py', findfile.relativepath)
        self.assertEqual('../findfile.py', str(findfile))


if __name__ == '__main__':
    unittest.main()
