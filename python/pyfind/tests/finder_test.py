# -*- coding: utf-8 -*-
################################################################################
#
# finder_test.py
#
# Finder testing
#
################################################################################
import os
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from pyfind import FileType, Finder, FindFile, FindSettings, SHAREDPATH


class FinderTest(unittest.TestCase):

    def get_settings(self):
        settings = FindSettings()
        settings.startpath = '.'
        settings.add_patterns('Finder', 'findpatterns')
        settings.debug = True
        return settings

    def get_test_file(self):
        return os.path.join(SHAREDPATH, 'testFiles/testFile2.txt')

################################################################################
# is_find_dir tests
################################################################################
    def test_is_find_dir_no_patterns(self):
        settings = self.get_settings()
        finder = Finder(settings)
        dir = 'plfind'
        self.assertTrue(finder.is_find_dir(dir))

    def test_is_find_dir_matches_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('plfind', 'in_dirpatterns')
        finder = Finder(settings)
        dir = 'plfind'
        self.assertTrue(finder.is_find_dir(dir))

    def test_is_find_dir_no_match_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('plfind', 'in_dirpatterns')
        finder = Finder(settings)
        dir = 'pyfind'
        self.assertFalse(finder.is_find_dir(dir))

    def test_is_find_dir_matches_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('pyfind', 'out_dirpatterns')
        finder = Finder(settings)
        dir = 'pyfind'
        self.assertFalse(finder.is_find_dir(dir))

    def test_is_find_dir_no_match_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('pyfind', 'out_dirpatterns')
        finder = Finder(settings)
        dir = 'plfind'
        self.assertTrue(finder.is_find_dir(dir))

    def test_is_find_dir_single_dot(self):
        settings = self.get_settings()
        finder = Finder(settings)
        dir = '.'
        self.assertTrue(finder.is_find_dir(dir))

    def test_is_find_dir_double_dot(self):
        settings = self.get_settings()
        finder = Finder(settings)
        dir = '..'
        self.assertTrue(finder.is_find_dir(dir))

    def test_is_find_dir_hidden_dir(self):
        settings = self.get_settings()
        finder = Finder(settings)
        dir = '.git'
        self.assertFalse(finder.is_find_dir(dir))

    def test_is_find_dir_hidden_dir_include_hidden(self):
        settings = self.get_settings()
        settings.excludehidden = False
        finder = Finder(settings)
        dir = '.git'
        self.assertTrue(finder.is_find_dir(dir))

################################################################################
# is_find_file tests
################################################################################
    def test_is_find_file_matches_by_default(self):
        settings = self.get_settings()
        finder = Finder(settings)
        f = FindFile(path='.', filename='FileUtil.pm', filetype=FileType.CODE)
        self.assertTrue(finder.is_find_file(f))

    def test_is_find_file_matches_in_extension(self):
        settings = self.get_settings()
        settings.add_exts('pm', 'in_extensions')
        finder = Finder(settings)
        f = FindFile(path='.', filename='FileUtil.pm', filetype=FileType.CODE)
        self.assertTrue(finder.is_find_file(f))

    def test_is_find_file_no_match_in_extension(self):
        settings = self.get_settings()
        settings.add_exts('pl', 'in_extensions')
        finder = Finder(settings)
        f = FindFile(path='.', filename='FileUtil.pm', filetype=FileType.CODE)
        self.assertFalse(finder.is_find_file(f))

    def test_is_find_file_matches_out_extension(self):
        settings = self.get_settings()
        settings.add_exts('pm', 'out_extensions')
        finder = Finder(settings)
        f = FindFile(path='.', filename='FileUtil.pm', filetype=FileType.CODE)
        self.assertFalse(finder.is_find_file(f))

    def test_is_find_file_no_match_out_extension(self):
        settings = self.get_settings()
        settings.add_exts('py', 'out_extensions')
        finder = Finder(settings)
        f = FindFile(path='.', filename='FileUtil.pm', filetype=FileType.CODE)
        self.assertTrue(finder.is_find_file(f))

    def test_is_find_file_matches_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('Find', 'in_filepatterns')
        finder = Finder(settings)
        f = FindFile(path='.', filename='Finder.pm', filetype=FileType.CODE)
        self.assertTrue(finder.is_find_file(f))

    def test_is_find_file_no_match_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('Find', 'in_filepatterns')
        finder = Finder(settings)
        f = FindFile(path='.', filename='FileUtil.pm', filetype=FileType.CODE)
        self.assertFalse(finder.is_find_file(f))

    def test_is_find_file_matches_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('Find', 'out_filepatterns')
        finder = Finder(settings)
        f = FindFile(path='.', filename='Finder.pm', filetype=FileType.CODE)
        self.assertFalse(finder.is_find_file(f))

    def test_is_find_file_no_match_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('Find', 'out_filepatterns')
        finder = Finder(settings)
        f = FindFile(path='.', filename='FileUtil.pm', filetype=FileType.CODE)
        self.assertTrue(finder.is_find_file(f))

################################################################################
# is__archive_find_file tests
################################################################################
    def test_is_archive_find_file_matches_by_default(self):
        settings = self.get_settings()
        finder = Finder(settings)
        f = 'archive.zip'
        self.assertTrue(finder.is_archive_find_file(f))

    def test_is_archive_find_file_matches_in_extension(self):
        settings = self.get_settings()
        settings.add_exts('zip', 'in_archiveextensions')
        finder = Finder(settings)
        f = 'archive.zip'
        self.assertTrue(finder.is_archive_find_file(f))

    def test_is_archive_find_file_no_match_in_extension(self):
        settings = self.get_settings()
        settings.add_exts('gz', 'in_archiveextensions')
        finder = Finder(settings)
        f = 'archive.zip'
        self.assertFalse(finder.is_archive_find_file(f))

    def test_is_archive_find_file_matches_out_extension(self):
        settings = self.get_settings()
        settings.add_exts('zip', 'out_archiveextensions')
        finder = Finder(settings)
        f = 'archive.zip'
        self.assertFalse(finder.is_archive_find_file(f))

    def test_is_archive_find_file_no_match_out_extension(self):
        settings = self.get_settings()
        settings.add_exts('gz', 'out_archiveextensions')
        finder = Finder(settings)
        f = 'archive.zip'
        self.assertTrue(finder.is_archive_find_file(f))

    def test_is_archive_find_file_matches_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('arch', 'in_archivefilepatterns')
        finder = Finder(settings)
        f = 'archive.zip'
        self.assertTrue(finder.is_archive_find_file(f))

    def test_is_archive_find_file_no_match_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('archives', 'in_archivefilepatterns')
        finder = Finder(settings)
        f = 'archive.zip'
        self.assertFalse(finder.is_archive_find_file(f))

    def test_is_archive_find_file_matches_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('arch', 'out_archivefilepatterns')
        finder = Finder(settings)
        f = 'archive.zip'
        self.assertFalse(finder.is_archive_find_file(f))

    def test_is_archive_find_file_no_match_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('archives', 'out_archivefilepatterns')
        finder = Finder(settings)
        f = 'archive.zip'
        self.assertTrue(finder.is_archive_find_file(f))

################################################################################
# filter_file tests
################################################################################
    def test_filter_file_matches_by_default(self):
        settings = self.get_settings()
        finder = Finder(settings)
        f = FindFile(path='', filename='FileUtil.pm', filetype=FileType.TEXT)
        self.assertTrue(finder.filter_file(f))

    def test_filter_file_is_find_file(self):
        settings = self.get_settings()
        settings.add_exts('pm', 'in_extensions')
        finder = Finder(settings)
        f = FindFile(path='', filename='FileUtil.pm', filetype=FileType.TEXT)
        self.assertTrue(finder.filter_file(f))

    def test_filter_file_not_is_find_file(self):
        settings = self.get_settings()
        settings.add_exts('pl', 'in_extensions')
        finder = Finder(settings)
        f = FindFile(path='', filename='FileUtil.pm', filetype=FileType.TEXT)
        self.assertFalse(finder.filter_file(f))

    def test_filter_file_is_hidden_file(self):
        settings = self.get_settings()
        finder = Finder(settings)
        f = FindFile(path='', filename='.gitignore', filetype=FileType.UNKNOWN)
        self.assertFalse(finder.filter_file(f))

    def test_filter_file_hidden_includehidden(self):
        settings = self.get_settings()
        settings.excludehidden = False
        finder = Finder(settings)
        f = FindFile(path='', filename='.gitignore', filetype=FileType.UNKNOWN)
        self.assertTrue(finder.filter_file(f))

    def test_filter_file_archive_no_findarchives(self):
        settings = self.get_settings()
        finder = Finder(settings)
        f = FindFile(path='', filename='archive.zip', filetype=FileType.ARCHIVE)
        self.assertFalse(finder.filter_file(f))

    def test_filter_file_archive_findarchives(self):
        settings = self.get_settings()
        settings.findarchives = 1
        finder = Finder(settings)
        f = FindFile(path='', filename='archive.zip', filetype=FileType.ARCHIVE)
        self.assertTrue(finder.filter_file(f))

    def test_filter_file_archive_archivesonly(self):
        settings = self.get_settings()
        settings.archivesonly = True
        settings.findarchives = True
        finder = Finder(settings)
        f = FindFile(path='', filename='archive.zip', filetype=FileType.ARCHIVE)
        self.assertTrue(finder.filter_file(f))

    def test_filter_file_nonarchive_archivesonly(self):
        settings = self.get_settings()
        settings.archivesonly = True
        settings.findarchives = True
        finder = Finder(settings)
        f = FindFile(path='', filename='FileUtil.pm', filetype=FileType.TEXT)
        self.assertFalse(finder.filter_file(f))

################################################################################
# find_lines tests
################################################################################
    def test_find_lines(self):
        settings = self.get_settings()
        finder = Finder(settings)
        testfile = self.get_test_file()
        results = []
        try:
            fo = open(testfile, 'r')
            results = finder.find_line_iterator(fo)
            fo.close()
        except IOError as e:
            print(('IOError: {0!s}'.format(e)))
        self.assertEqual(len(results), 2)

        firstResult = results[0]
        self.assertEqual(firstResult.linenum, 29)
        self.assertEqual(firstResult.match_start_index, 3)
        self.assertEqual(firstResult.match_end_index, 11)

        secondResult = results[1]
        self.assertEqual(secondResult.linenum, 35)
        self.assertEqual(secondResult.match_start_index, 24)
        self.assertEqual(secondResult.match_end_index, 32)

################################################################################
# find_multiline_string tests
################################################################################
    def test_find_multiline_string(self):
        settings = self.get_settings()
        finder = Finder(settings)
        testfile = self.get_test_file()
        results = []
        try:
            fo = open(testfile, 'r')
            contents = fo.read()
            results = finder.find_multiline_string(contents)
            fo.close()
        except IOError as e:
            print(('IOError: {0!s}'.format(e)))
        self.assertEqual(len(results), 2)

        firstResult = results[0]
        self.assertEqual(firstResult.linenum, 29)
        self.assertEqual(firstResult.match_start_index, 3)
        self.assertEqual(firstResult.match_end_index, 11)

        secondResult = results[1]
        self.assertEqual(secondResult.linenum, 35)
        self.assertEqual(secondResult.match_start_index, 24)
        self.assertEqual(secondResult.match_end_index, 32)


if __name__ == '__main__':
    unittest.main()
