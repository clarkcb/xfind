# -*- coding: utf-8 -*-
################################################################################
#
# finder_test.py
#
# Finder testing
#
################################################################################
import os
from pathlib import Path
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)[:-6]))

from pyfind import Finder, FindSettings, XFINDPATH, SHAREDPATH


class FinderTest(unittest.TestCase):
    def get_settings(self):
        settings = FindSettings()
        settings.add_paths('.')
        settings.debug = True
        return settings

    def get_test_file(self):
        return os.path.join(SHAREDPATH, 'testFiles/testFile2.txt')

    def ensure_archive(self):
        archive_path = os.path.join(SHAREDPATH, 'testFiles/archive.zip')
        if not os.path.exists(archive_path):
            Path(archive_path).touch()

################################################################################
# is_matching_dir tests
################################################################################
    def test_is_matching_dir_no_patterns(self):
        settings = self.get_settings()
        finder = Finder(settings)
        dir = 'plfind'
        self.assertTrue(finder.is_matching_dir(dir))

    def test_is_matching_dir_matches_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('plfind', 'in_dirpatterns')
        finder = Finder(settings)
        dir = 'plfind'
        self.assertTrue(finder.is_matching_dir(dir))

    def test_is_matching_dir_no_match_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('plfind', 'in_dirpatterns')
        finder = Finder(settings)
        dir = 'pyfind'
        self.assertFalse(finder.is_matching_dir(dir))

    def test_is_matching_dir_matches_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('pyfind', 'out_dirpatterns')
        finder = Finder(settings)
        dir = 'pyfind'
        self.assertFalse(finder.is_matching_dir(dir))

    def test_is_matching_dir_no_match_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('pyfind', 'out_dirpatterns')
        finder = Finder(settings)
        dir = 'plfind'
        self.assertTrue(finder.is_matching_dir(dir))

    def test_is_matching_dir_single_dot(self):
        settings = self.get_settings()
        finder = Finder(settings)
        dir = '.'
        self.assertTrue(finder.is_matching_dir(dir))

    def test_is_matching_dir_double_dot(self):
        settings = self.get_settings()
        finder = Finder(settings)
        dir = '..'
        self.assertTrue(finder.is_matching_dir(dir))

    def test_is_matching_dir_hidden_dir(self):
        settings = self.get_settings()
        finder = Finder(settings)
        dir = '.git'
        self.assertFalse(finder.is_matching_dir(dir))

    def test_is_matching_dir_hidden_dir_include_hidden(self):
        settings = self.get_settings()
        settings.excludehidden = False
        finder = Finder(settings)
        dir = '.git'
        self.assertTrue(finder.is_matching_dir(dir))

################################################################################
# filter_to_file_result tests
#
# NOTE: the filepaths are required to be existing for these tests to work
################################################################################
    def test_filter_to_file_result_matches_by_default(self):
        settings = self.get_settings()
        finder = Finder(settings)
        filepath = os.path.join(XFINDPATH, 'python/pyfind/pyfind/fileutil.py')
        fileresult = finder.filter_to_file_result(filepath)
        self.assertIsNotNone(fileresult)

    def test_filter_to_file_result_is_find_file(self):
        settings = self.get_settings()
        settings.add_exts('py', 'in_extensions')
        finder = Finder(settings)
        filepath = os.path.join(XFINDPATH, 'python/pyfind/pyfind/fileutil.py')
        fileresult = finder.filter_to_file_result(filepath)
        self.assertIsNotNone(fileresult)

    def test_filter_to_file_result_not_is_find_file(self):
        settings = self.get_settings()
        settings.add_exts('pl', 'in_extensions')
        finder = Finder(settings)
        filepath = os.path.join(XFINDPATH, 'python/pyfind/pyfind/fileutil.py')
        fileresult = finder.filter_to_file_result(filepath)
        self.assertIsNone(fileresult)

    def test_filter_to_file_result_is_hidden_file(self):
        settings = self.get_settings()
        finder = Finder(settings)
        filepath = '{}/python/pyfind/.gitignore'.format(XFINDPATH)
        fileresult = finder.filter_to_file_result(filepath)
        self.assertIsNone(fileresult)

    def test_filter_to_file_result_hidden_includehidden(self):
        settings = self.get_settings()
        settings.excludehidden = False
        finder = Finder(settings)
        filepath = '{}/python/pyfind/.gitignore'.format(XFINDPATH)
        fileresult = finder.filter_to_file_result(filepath)
        self.assertIsNotNone(fileresult)

    def test_filter_to_file_result_archive_no_match_by_default(self):
        self.ensure_archive()
        settings = self.get_settings()
        finder = Finder(settings)
        archive_path = os.path.join(SHAREDPATH, 'testFiles/archive.zip')
        fileresult = finder.filter_to_file_result(archive_path)
        self.assertIsNone(fileresult)

    def test_filter_to_file_result_archive_no_includearchives(self):
        self.ensure_archive()
        settings = self.get_settings()
        finder = Finder(settings)
        fileresult = finder.filter_to_file_result('./archive.zip')
        self.assertIsNone(fileresult)

    def test_filter_to_file_result_archive_includearchives(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.includearchives = True
        finder = Finder(settings)
        archive_path = os.path.join(SHAREDPATH, 'testFiles/archive.zip')
        fileresult = finder.filter_to_file_result(archive_path)
        self.assertIsNotNone(fileresult)

    def test_filter_to_file_result_archive_matches_in_extension(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.includearchives = True
        settings.add_exts('zip', 'in_archiveextensions')
        finder = Finder(settings)
        archive_path = os.path.join(SHAREDPATH, 'testFiles/archive.zip')
        fileresult = finder.filter_to_file_result(archive_path)
        self.assertIsNotNone(fileresult)

    def test_filter_to_file_result_archive_no_match_in_extension(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.includearchives = True
        settings.add_exts('gz', 'in_archiveextensions')
        finder = Finder(settings)
        archive_path = os.path.join(SHAREDPATH, 'testFiles/archive.zip')
        fileresult = finder.filter_to_file_result(archive_path)
        self.assertIsNone(fileresult)

    def test_filter_to_file_result_archive_matches_out_extension(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.includearchives = True
        settings.add_exts('zip', 'out_archiveextensions')
        finder = Finder(settings)
        archive_path = os.path.join(SHAREDPATH, 'testFiles/archive.zip')
        fileresult = finder.filter_to_file_result(archive_path)
        self.assertIsNone(fileresult)

    def test_filter_to_file_result_archive_no_match_out_extension(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.includearchives = True
        settings.add_exts('gz', 'out_archiveextensions')
        finder = Finder(settings)
        archive_path = os.path.join(SHAREDPATH, 'testFiles/archive.zip')
        fileresult = finder.filter_to_file_result(archive_path)
        self.assertIsNotNone(fileresult)

    def test_filter_to_file_result_archive_matches_in_pattern(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.includearchives = True
        settings.add_patterns('arch', 'in_archivefilepatterns')
        finder = Finder(settings)
        archive_path = os.path.join(SHAREDPATH, 'testFiles/archive.zip')
        fileresult = finder.filter_to_file_result(archive_path)
        self.assertIsNotNone(fileresult)

    def test_filter_to_file_result_archive_no_match_in_pattern(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.includearchives = True
        settings.add_patterns('archives', 'in_archivefilepatterns')
        finder = Finder(settings)
        archive_path = os.path.join(SHAREDPATH, 'testFiles/archive.zip')
        fileresult = finder.filter_to_file_result(archive_path)
        self.assertIsNone(fileresult)

    def test_filter_to_file_result_archive_matches_out_pattern(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.includearchives = True
        settings.add_patterns('arch', 'out_archivefilepatterns')
        finder = Finder(settings)
        archive_path = os.path.join(SHAREDPATH, 'testFiles/archive.zip')
        fileresult = finder.filter_to_file_result(archive_path)
        self.assertIsNone(fileresult)

    def test_filter_to_file_result_archive_no_match_out_pattern(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.includearchives = True
        settings.add_patterns('archives', 'out_archivefilepatterns')
        finder = Finder(settings)
        archive_path = os.path.join(SHAREDPATH, 'testFiles/archive.zip')
        fileresult = finder.filter_to_file_result(archive_path)
        self.assertIsNotNone(fileresult)

    def test_filter_to_file_result_archive_archivesonly(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.set_property('archivesonly', True)
        finder = Finder(settings)
        archive_path = os.path.join(SHAREDPATH, 'testFiles/archive.zip')
        fileresult = finder.filter_to_file_result(archive_path)
        self.assertIsNotNone(fileresult)

    def test_filter_to_file_result_nonarchive_archivesonly(self):
        settings = self.get_settings()
        settings.set_property('archivesonly', True)
        finder = Finder(settings)
        filepath = os.path.join(XFINDPATH, 'python/pyfind/pyfind/fileutil.py')
        fileresult = finder.filter_to_file_result(filepath)
        self.assertIsNone(fileresult)


if __name__ == '__main__':
    unittest.main()
