# -*- coding: utf-8 -*-
"""
################################################################################
#
# finder_test.py
#
# Finder testing
#
################################################################################
"""
import os
from pathlib import Path
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)[:-6]))

from pyfind import Finder, FindSettings, XFIND_PATH, SHARED_PATH


class FinderTest(unittest.TestCase):
    def get_settings(self):
        settings = FindSettings()
        settings.add_paths('.')
        settings.debug = True
        return settings

    def ensure_archive(self):
        archive_path = Path(SHARED_PATH, 'testFiles/archive.zip')
        if not os.path.exists(archive_path):
            Path(archive_path).touch()

################################################################################
# is_matching_dir_path tests
################################################################################
    def test_is_matching_dir_path_no_patterns(self):
        settings = self.get_settings()
        finder = Finder(settings)
        d = Path('plfind')
        self.assertTrue(finder.is_matching_dir_path(d))

    def test_is_matching_dir_path_matches_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('plfind', 'in_dir_patterns')
        finder = Finder(settings)
        d = Path('plfind')
        self.assertTrue(finder.is_matching_dir_path(d))

    def test_is_matching_dir_path_no_match_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('plfind', 'in_dir_patterns')
        finder = Finder(settings)
        d = Path('pyfind')
        self.assertFalse(finder.is_matching_dir_path(d))

    def test_is_matching_dir_path_matches_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('pyfind', 'out_dir_patterns')
        finder = Finder(settings)
        d = Path('pyfind')
        self.assertFalse(finder.is_matching_dir_path(d))

    def test_is_matching_dir_path_no_match_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('pyfind', 'out_dir_patterns')
        finder = Finder(settings)
        d = Path('plfind')
        self.assertTrue(finder.is_matching_dir_path(d))

    def test_is_matching_dir_path_single_dot(self):
        settings = self.get_settings()
        finder = Finder(settings)
        d = Path('.')
        self.assertTrue(finder.is_matching_dir_path(d))

    def test_is_matching_dir_path_double_dot(self):
        settings = self.get_settings()
        finder = Finder(settings)
        d = Path('..')
        self.assertTrue(finder.is_matching_dir_path(d))

    def test_is_matching_dir_path_hidden_dir_path(self):
        settings = self.get_settings()
        finder = Finder(settings)
        d = Path('.git')
        self.assertFalse(finder.is_matching_dir_path(d))

    def test_is_matching_dir_path_hidden_dir_path_include_hidden(self):
        settings = self.get_settings()
        settings.include_hidden = True
        finder = Finder(settings)
        d = Path('.git')
        self.assertTrue(finder.is_matching_dir_path(d))

################################################################################
# filter_to_file_result tests
#
# NOTE: the filepaths are required to be existing for these tests to work
################################################################################
    def test_filter_to_file_result_matches_by_default(self):
        settings = self.get_settings()
        finder = Finder(settings)
        file_path = Path(XFIND_PATH, 'python/pyfind/pyfind/fileutil.py')
        file_result = finder.filter_to_file_result(file_path)
        self.assertIsNotNone(file_result)

    def test_filter_to_file_result_is_find_file(self):
        settings = self.get_settings()
        settings.add_strs_to_set('py', 'in_extensions')
        finder = Finder(settings)
        file_path = Path(XFIND_PATH, 'python/pyfind/pyfind/fileutil.py')
        file_result = finder.filter_to_file_result(file_path)
        self.assertIsNotNone(file_result)

    def test_filter_to_file_result_not_is_find_file(self):
        settings = self.get_settings()
        settings.add_strs_to_set('pl', 'in_extensions')
        finder = Finder(settings)
        file_path = Path(XFIND_PATH, 'python/pyfind/pyfind/fileutil.py')
        file_result = finder.filter_to_file_result(file_path)
        self.assertIsNone(file_result)

    def test_filter_to_file_result_is_hidden_file(self):
        settings = self.get_settings()
        finder = Finder(settings)
        file_path = Path(XFIND_PATH, 'python/pyfind/.gitignore')
        file_result = finder.filter_to_file_result(file_path)
        self.assertIsNone(file_result)

    def test_filter_to_file_result_hidden_include_hidden(self):
        settings = self.get_settings()
        settings.include_hidden = True
        finder = Finder(settings)
        file_path = Path(XFIND_PATH, 'python/pyfind/.gitignore')
        file_result = finder.filter_to_file_result(file_path)
        self.assertIsNotNone(file_result)

    def test_filter_to_file_result_archive_no_match_by_default(self):
        self.ensure_archive()
        settings = self.get_settings()
        finder = Finder(settings)
        archive_path = Path(SHARED_PATH, 'testFiles/archive.zip')
        file_result = finder.filter_to_file_result(archive_path)
        self.assertIsNone(file_result)

    def test_filter_to_file_result_archive_no_include_archives(self):
        self.ensure_archive()
        settings = self.get_settings()
        finder = Finder(settings)
        archive_path = Path('./archive.zip')
        file_result = finder.filter_to_file_result(archive_path)
        self.assertIsNone(file_result)

    def test_filter_to_file_result_archive_include_archives(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.include_archives = True
        finder = Finder(settings)
        archive_path = Path(SHARED_PATH, 'testFiles/archive.zip')
        file_result = finder.filter_to_file_result(archive_path)
        self.assertIsNotNone(file_result)

    def test_filter_to_file_result_archive_matches_in_extension(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.include_archives = True
        settings.add_strs_to_set('zip', 'in_archive_extensions')
        finder = Finder(settings)
        archive_path = Path(SHARED_PATH, 'testFiles/archive.zip')
        file_result = finder.filter_to_file_result(archive_path)
        self.assertIsNotNone(file_result)

    def test_filter_to_file_result_archive_no_match_in_extension(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.include_archives = True
        settings.add_strs_to_set('gz', 'in_archive_extensions')
        finder = Finder(settings)
        archive_path = Path(SHARED_PATH, 'testFiles/archive.zip')
        file_result = finder.filter_to_file_result(archive_path)
        self.assertIsNone(file_result)

    def test_filter_to_file_result_archive_matches_out_extension(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.include_archives = True
        settings.add_strs_to_set('zip', 'out_archive_extensions')
        finder = Finder(settings)
        archive_path = Path(SHARED_PATH, 'testFiles/archive.zip')
        file_result = finder.filter_to_file_result(archive_path)
        self.assertIsNone(file_result)

    def test_filter_to_file_result_archive_no_match_out_extension(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.include_archives = True
        settings.add_strs_to_set('gz', 'out_archive_extensions')
        finder = Finder(settings)
        archive_path = Path(SHARED_PATH, 'testFiles/archive.zip')
        file_result = finder.filter_to_file_result(archive_path)
        self.assertIsNotNone(file_result)

    def test_filter_to_file_result_archive_matches_in_pattern(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.include_archives = True
        settings.add_patterns('arch', 'in_archive_file_patterns')
        finder = Finder(settings)
        archive_path = Path(SHARED_PATH, 'testFiles/archive.zip')
        file_result = finder.filter_to_file_result(archive_path)
        self.assertIsNotNone(file_result)

    def test_filter_to_file_result_archive_no_match_in_pattern(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.include_archives = True
        settings.add_patterns('archives', 'in_archive_file_patterns')
        finder = Finder(settings)
        archive_path = Path(SHARED_PATH, 'testFiles/archive.zip')
        file_result = finder.filter_to_file_result(archive_path)
        self.assertIsNone(file_result)

    def test_filter_to_file_result_archive_matches_out_pattern(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.include_archives = True
        settings.add_patterns('arch', 'out_archive_file_patterns')
        finder = Finder(settings)
        archive_path = Path(SHARED_PATH, 'testFiles/archive.zip')
        file_result = finder.filter_to_file_result(archive_path)
        self.assertIsNone(file_result)

    def test_filter_to_file_result_archive_no_match_out_pattern(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.include_archives = True
        settings.add_patterns('archives', 'out_archive_file_patterns')
        finder = Finder(settings)
        archive_path = Path(SHARED_PATH, 'testFiles/archive.zip')
        file_result = finder.filter_to_file_result(archive_path)
        self.assertIsNotNone(file_result)

    def test_filter_to_file_result_archive_archives_only(self):
        self.ensure_archive()
        settings = self.get_settings()
        settings.set_property('archives_only', True)
        finder = Finder(settings)
        archive_path = Path(SHARED_PATH, 'testFiles/archive.zip')
        file_result = finder.filter_to_file_result(archive_path)
        self.assertIsNotNone(file_result)

    def test_filter_to_file_result_nonarchive_archives_only(self):
        settings = self.get_settings()
        settings.set_property('archives_only', True)
        finder = Finder(settings)
        file_path = Path(XFIND_PATH, 'python/pyfind/pyfind/fileutil.py')
        file_result = finder.filter_to_file_result(file_path)
        self.assertIsNone(file_result)

################################################################################
# test filtering symlink files
################################################################################
    def test_default_no_symlinks(self):
        settings = FindSettings()
        settings.add_path(Path(XFIND_PATH, 'bin'))
        finder = Finder(settings)
        file_results = finder.find_files()
        self.assertTrue(len(file_results) < 4)

    def test_follow_symlinks(self):
        settings = FindSettings()
        settings.add_path(Path(XFIND_PATH, 'bin'))
        settings.follow_symlinks = True
        finder = Finder(settings)
        file_results = finder.find_files()
        self.assertTrue(len(file_results) == 0 or len(file_results) > 2)

    def test_no_follow_symlinks(self):
        settings = FindSettings()
        settings.add_path(Path(XFIND_PATH, 'bin'))
        settings.follow_symlinks = False
        finder = Finder(settings)
        file_results = finder.find_files()
        self.assertTrue(len(file_results) < 4)


if __name__ == '__main__':
    unittest.main()
