# -*- coding: utf-8 -*-
################################################################################
#
# findoptions_test.py
#
# class FindOptionsTest: testing of FindOptions class
#
################################################################################
import os
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from pyfind import FindException, FindOptions, FindSettings


class FindOptionsTest(unittest.TestCase):
    @classmethod
    def setUpClass(self):
        self.findoptions = FindOptions()

    def test_no_args(self):
        # test the props
        settings = self.findoptions.find_settings_from_args([])
        self.assertFalse(settings.archivesonly)
        self.assertFalse(settings.debug)
        self.assertTrue(settings.excludehidden)
        self.assertFalse(settings.includearchives)
        self.assertFalse(settings.listdirs)
        self.assertTrue(settings.listfiles)
        self.assertFalse(settings.printusage)
        self.assertFalse(settings.printversion)
        self.assertTrue(settings.recursive)
        self.assertFalse(settings.verbose)
        # test the extension and pattern sets
        self.assertFalse(settings.in_archiveextensions)
        self.assertFalse(settings.in_archivefilepatterns)
        self.assertFalse(settings.in_dirpatterns)
        self.assertFalse(settings.in_filepatterns)
        self.assertFalse(settings.out_archiveextensions)
        self.assertFalse(settings.out_archivefilepatterns)
        self.assertFalse(settings.out_dirpatterns)
        self.assertFalse(settings.out_filepatterns)

    def test_valid_args(self):
        args = ['-x', 'py,rb', '.']
        settings = self.findoptions.find_settings_from_args(args)
        for x in {'py', 'rb'}:
            self.assertIn(x, settings.in_extensions)
        self.assertEqual(1, len(settings.paths))
        self.assertIn('.', settings.paths)

    def test_archivesonly_arg(self):
        args = ['--archivesonly']
        settings = self.findoptions.find_settings_from_args(args)
        self.assertTrue(settings.archivesonly)
        self.assertTrue(settings.includearchives)

    def test_debug_arg(self):
        args = ['--debug']
        settings = self.findoptions.find_settings_from_args(args)
        self.assertTrue(settings.debug)
        self.assertTrue(settings.verbose)

    def test_missing_arg(self):
        args = ['-x', 'py,rb', '.', '-D']
        with self.assertRaises(FindException) as cm:
            settings = self.findoptions.find_settings_from_args(args)
        self.assertEqual(str(cm.exception), 'Missing value for option D')

    def test_invalid_arg(self):
        args = ['-x', 'py,rb', '.', '-Q']
        with self.assertRaises(FindException) as cm:
            settings = self.findoptions.find_settings_from_args(args)
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
        self.findoptions.settings_from_json(json, settings)
        self.assertEqual(1, len(settings.paths))
        self.assertIn('~/src/xfind/', settings.paths)
        for x in {'js', 'ts'}:
            self.assertIn(x, settings.in_extensions)
        self.assertEqual(list(settings.out_dirpatterns)[0].pattern, 'node_module')
        self.assertEqual(list(settings.out_filepatterns)[0].pattern, 'temp')
        self.assertTrue(settings.debug)
        self.assertTrue(settings.verbose)
        self.assertFalse(settings.excludehidden)


if __name__ == '__main__':
    unittest.main()
