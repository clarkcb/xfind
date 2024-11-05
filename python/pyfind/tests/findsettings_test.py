# -*- coding: utf-8 -*-
"""
################################################################################
#
# findsettings_test.py
#
# class FindSettingsTest: testing of FindSettings class
#
################################################################################
"""
import os
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)[:-6]))

from pyfind import FindSettings


class FindSettingsTest(unittest.TestCase):
    def setUp(self):
        self.settings = FindSettings()

    def test_default_settings(self):
        # test the props
        self.assertFalse(self.settings.archives_only)
        self.assertFalse(self.settings.debug)
        self.assertFalse(self.settings.follow_symlinks)
        self.assertFalse(self.settings.include_archives)
        self.assertFalse(self.settings.include_hidden)
        self.assertFalse(self.settings.print_dirs)
        self.assertFalse(self.settings.print_files)
        self.assertFalse(self.settings.print_usage)
        self.assertFalse(self.settings.print_version)
        self.assertTrue(self.settings.recursive)
        self.assertFalse(self.settings.sort_case_insensitive)
        self.assertFalse(self.settings.sort_descending)
        self.assertFalse(self.settings.verbose)
        # test the extension and pattern sets
        self.assertFalse(self.settings.in_archive_extensions)
        self.assertFalse(self.settings.in_archive_file_patterns)
        self.assertFalse(self.settings.in_dir_patterns)
        self.assertFalse(self.settings.in_file_patterns)
        self.assertFalse(self.settings.out_archive_extensions)
        self.assertFalse(self.settings.out_archive_file_patterns)
        self.assertFalse(self.settings.out_dir_patterns)
        self.assertFalse(self.settings.out_file_patterns)

    def test_set_properties(self):
        props = {
            'archives_only': True,
            'debug': True,
            'follow_symlinks': True,
            'include_archives': True,
            'include_hidden': True,
            'print_dirs': True,
            'print_files': True,
            'print_usage': True,
            'print_version': True,
            'recursive': False,
            'verbose': True,
        }
        self.settings.set_properties(props)
        self.assertTrue(self.settings.archives_only)
        self.assertTrue(self.settings.debug)
        self.assertTrue(self.settings.follow_symlinks)
        self.assertTrue(self.settings.include_archives)
        self.assertTrue(self.settings.include_hidden)
        self.assertTrue(self.settings.print_dirs)
        self.assertTrue(self.settings.print_files)
        self.assertTrue(self.settings.print_usage)
        self.assertTrue(self.settings.print_version)
        self.assertFalse(self.settings.recursive)
        self.assertTrue(self.settings.verbose)

    def test_add_single_extension(self):
        self.settings.add_strs_to_set('py', 'in_extensions')
        self.assertEqual(1, len(self.settings.in_extensions))
        self.assertIn('py', self.settings.in_extensions)

    def test_add_comma_delimited_extensions(self):
        self.settings.add_strs_to_set('py,rb,scala', 'in_extensions')
        self.assertEqual(3, len(self.settings.in_extensions))
        for x in {'py', 'rb', 'scala'}:
            self.assertIn(x, self.settings.in_extensions)

    def test_add_extensions_set(self):
        extensions_set = {'py', 'rb', 'scala'}
        self.settings.add_strs_to_set(extensions_set, 'in_extensions')
        self.assertEqual(3, len(self.settings.in_extensions))
        for x in extensions_set:
            self.assertIn(x, self.settings.in_extensions)


if __name__ == '__main__':
    unittest.main()
