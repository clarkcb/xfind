# -*- coding: utf-8 -*-
"""
################################################################################
#
# findoptions_test.py
#
# class FindOptionsTest: testing of FindOptions class
#
################################################################################
"""
import os
from pathlib import Path
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)[:-6]))

from pyfind import FindException, FindOptions, FindSettings


class FindOptionsTest(unittest.TestCase):
    @classmethod
    def setUpClass(self):
        self.find_options = FindOptions()

    def test_no_args(self):
        # test the props
        settings = self.find_options.find_settings_from_args([])
        self.assertFalse(settings.archives_only)
        self.assertFalse(settings.debug)
        self.assertFalse(settings.include_archives)
        self.assertFalse(settings.include_hidden)
        self.assertFalse(settings.print_dirs)
        self.assertTrue(settings.print_files)
        self.assertFalse(settings.print_usage)
        self.assertFalse(settings.print_version)
        self.assertTrue(settings.recursive)
        self.assertFalse(settings.verbose)
        # test the extension and pattern sets
        self.assertFalse(settings.in_archive_extensions)
        self.assertFalse(settings.in_archive_file_patterns)
        self.assertFalse(settings.in_dir_patterns)
        self.assertFalse(settings.in_file_patterns)
        self.assertFalse(settings.out_archive_extensions)
        self.assertFalse(settings.out_archive_file_patterns)
        self.assertFalse(settings.out_dir_patterns)
        self.assertFalse(settings.out_file_patterns)

    def test_valid_args(self):
        args = ['-x', 'py,rb', '.']
        settings = self.find_options.find_settings_from_args(args)
        for x in {'py', 'rb'}:
            self.assertIn(x, settings.in_extensions)
        self.assertEqual(1, len(settings.paths))
        self.assertIn(Path('.'), settings.paths)

    def test_archives_only_arg(self):
        args = ['--archivesonly']
        settings = self.find_options.find_settings_from_args(args)
        self.assertTrue(settings.archives_only)
        self.assertTrue(settings.include_archives)

    def test_debug_arg(self):
        args = ['--debug']
        settings = self.find_options.find_settings_from_args(args)
        self.assertTrue(settings.debug)
        self.assertTrue(settings.verbose)

    def test_missing_arg(self):
        args = ['-x', 'py,rb', '.', '-D']
        with self.assertRaises(FindException) as cm:
            self.find_options.find_settings_from_args(args)
        self.assertEqual(str(cm.exception), 'Missing value for option D')

    def test_invalid_arg(self):
        args = ['-x', 'py,rb', '.', '-Q']
        with self.assertRaises(FindException) as cm:
            self.find_options.find_settings_from_args(args)
        self.assertEqual(str(cm.exception), 'Invalid option: Q')

    def test_settings_from_json(self):
        settings = FindSettings()
        json = '''{
  "path": "~/src/xfind/",
  "in-ext": ["js","ts"],
  "out-dirpattern": "node_module",
  "out-filepattern": ["temp"],
  "debug": true,
  "includehidden": true
}'''
        self.find_options.settings_from_json(json, settings)
        self.assertEqual(1, len(settings.paths))
        self.assertIn(Path('~/src/xfind/'), settings.paths)
        for x in {'js', 'ts'}:
            self.assertIn(x, settings.in_extensions)
        self.assertEqual(list(settings.out_dir_patterns)[0].pattern, 'node_module')
        self.assertEqual(list(settings.out_file_patterns)[0].pattern, 'temp')
        self.assertTrue(settings.debug)
        self.assertTrue(settings.verbose)
        self.assertTrue(settings.include_hidden)


if __name__ == '__main__':
    unittest.main()
