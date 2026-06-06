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
import tempfile
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
# is_included_dir_path tests
################################################################################
    def test_is_included_dir_path_no_patterns(self):
        settings = self.get_settings()
        finder = Finder(settings)
        d = Path('plfind')
        self.assertTrue(finder.is_matching_dir_path(d))

    def test_is_included_dir_path_matches_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('plfind', 'in_dir_patterns')
        finder = Finder(settings)
        d = Path('plfind')
        self.assertTrue(finder.is_matching_dir_path(d))

    def test_is_included_dir_path_no_match_in_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('plfind', 'in_dir_patterns')
        finder = Finder(settings)
        d = Path('pyfind')
        self.assertFalse(finder.is_matching_dir_path(d))

    def test_is_included_dir_path_matches_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('pyfind', 'out_dir_patterns')
        finder = Finder(settings)
        d = Path('pyfind')
        self.assertFalse(finder.is_matching_dir_path(d))

    def test_is_included_dir_path_no_match_out_pattern(self):
        settings = self.get_settings()
        settings.add_patterns('pyfind', 'out_dir_patterns')
        finder = Finder(settings)
        d = Path('plfind')
        self.assertTrue(finder.is_matching_dir_path(d))

    def test_is_included_dir_path_single_dot(self):
        settings = self.get_settings()
        finder = Finder(settings)
        d = Path('.')
        self.assertTrue(finder.is_matching_dir_path(d))

    def test_is_included_dir_path_double_dot(self):
        settings = self.get_settings()
        finder = Finder(settings)
        d = Path('..')
        self.assertTrue(finder.is_matching_dir_path(d))

    def test_is_included_dir_path_hidden_dir_path(self):
        settings = self.get_settings()
        finder = Finder(settings)
        d = Path('.git')
        self.assertFalse(finder.is_matching_dir_path(d))

    def test_is_included_dir_path_hidden_dir_path_include_hidden(self):
        settings = self.get_settings()
        settings.include_hidden = True
        finder = Finder(settings)
        d = Path('.git')
        self.assertTrue(finder.is_matching_dir_path(d))

    def test_is_traversable_dir_path_ignores_in_patterns(self):
        settings = self.get_settings()
        settings.add_patterns('keep', 'in_dir_patterns')
        finder = Finder(settings)
        self.assertTrue(finder.is_traversable_dir_path(Path('other')))

    def test_is_included_dir_path_applies_in_patterns(self):
        settings = self.get_settings()
        settings.add_patterns('keep', 'in_dir_patterns')
        finder = Finder(settings)
        self.assertFalse(finder.is_matching_dir_path(Path('other')))

    def test_find_files_traverses_through_nonmatching_ancestor_dir(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            target_file = root / 'outer' / 'keep' / 'target.txt'
            target_file.parent.mkdir(parents=True)
            target_file.touch()

            settings = FindSettings()
            settings.add_path(root)
            settings.add_patterns('keep', 'in_dir_patterns')
            finder = Finder(settings)

            file_results = finder.find_files()
            result_paths = {fr.path for fr in file_results}
            self.assertIn(target_file, result_paths)

    def test_find_files_out_pattern_prunes_even_if_descendant_matches_in_pattern(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            pruned_file = root / 'skip' / 'keep' / 'target.txt'
            pruned_file.parent.mkdir(parents=True)
            pruned_file.touch()

            settings = FindSettings()
            settings.add_path(root)
            settings.add_patterns('keep', 'in_dir_patterns')
            settings.add_patterns('skip', 'out_dir_patterns')
            finder = Finder(settings)

            file_results = finder.find_files()
            result_paths = {fr.path for fr in file_results}
            self.assertNotIn(pruned_file, result_paths)

    def test_find_files_hidden_dir_phased_include_hidden(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            root = Path(tmpdir)
            target_file = root / '.hidden' / 'keep' / 'target.txt'
            target_file.parent.mkdir(parents=True)
            target_file.touch()

            hidden_off_settings = FindSettings()
            hidden_off_settings.add_path(root)
            hidden_off_settings.add_patterns('keep', 'in_dir_patterns')
            hidden_off_settings.include_hidden = False
            hidden_off_finder = Finder(hidden_off_settings)
            hidden_off_results = {fr.path for fr in hidden_off_finder.find_files()}
            self.assertNotIn(target_file, hidden_off_results)

            hidden_on_settings = FindSettings()
            hidden_on_settings.add_path(root)
            hidden_on_settings.add_patterns('keep', 'in_dir_patterns')
            hidden_on_settings.include_hidden = True
            hidden_on_finder = Finder(hidden_on_settings)
            hidden_on_results = {fr.path for fr in hidden_on_finder.find_files()}
            self.assertIn(target_file, hidden_on_results)

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
