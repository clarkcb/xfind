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
        self.assertFalse(settings.firstmatch)
        self.assertTrue(settings.excludehidden)
        self.assertEqual(settings.linesafter, 0)
        self.assertEqual(settings.linesbefore, 0)
        self.assertFalse(settings.listdirs)
        self.assertFalse(settings.listfiles)
        self.assertFalse(settings.listlines)
        self.assertEqual(settings.maxlinelength, 150)
        self.assertFalse(settings.multilineoption-REMOVE)
        self.assertTrue(settings.printresults)
        self.assertFalse(settings.printusage)
        self.assertFalse(settings.printversion)
        self.assertTrue(settings.recursive)
        self.assertFalse(settings.findarchives)
        self.assertFalse(settings.uniquelines)
        self.assertFalse(settings.verbose)
        # test the extension and pattern sets
        self.assertFalse(settings.in_archiveextensions)
        self.assertFalse(settings.in_archivefilepatterns)
        self.assertFalse(settings.in_dirpatterns)
        self.assertFalse(settings.in_filepatterns)
        self.assertFalse(settings.in_linesafterpatterns)
        self.assertFalse(settings.in_linesbeforepatterns)
        self.assertFalse(settings.linesaftertopatterns)
        self.assertFalse(settings.linesafteruntilpatterns)
        self.assertFalse(settings.out_archiveextensions)
        self.assertFalse(settings.out_archivefilepatterns)
        self.assertFalse(settings.out_dirpatterns)
        self.assertFalse(settings.out_filepatterns)
        self.assertFalse(settings.out_linesafterpatterns)
        self.assertFalse(settings.out_linesbeforepatterns)
        self.assertFalse(settings.findpatterns)

    def test_valid_args(self):
        args = ['-x', 'py,rb', '-s', 'Find', '.']
        settings = self.findoptions.find_settings_from_args(args)
        self.assertEqual(settings.startpath, '.')
        for x in {'py', 'rb'}:
            self.assertIn(x, settings.in_extensions)
        self.assertEqual(list(settings.findpatterns)[0].pattern, 'Find')

    def test_archivesonly_arg(self):
        args = ['--archivesonly']
        settings = self.findoptions.find_settings_from_args(args)
        self.assertTrue(settings.archivesonly)
        self.assertTrue(settings.findarchives)

    def test_debug_arg(self):
        args = ['--debug']
        settings = self.findoptions.find_settings_from_args(args)
        self.assertTrue(settings.debug)
        self.assertTrue(settings.verbose)

    def test_missing_arg(self):
        args = ['-x', 'py,rb', '-s', 'Find', '.', '-D']
        with self.assertRaises(FindException) as cm:
            settings = self.findoptions.find_settings_from_args(args)
        self.assertEqual(str(cm.exception), 'Missing value for option D')

    def test_invalid_arg(self):
        args = ['-x', 'py,rb', '-s', 'Find', '.', '-Q']
        with self.assertRaises(FindException) as cm:
            settings = self.findoptions.find_settings_from_args(args)
        self.assertEqual(str(cm.exception), 'Invalid option: Q')

    def test_settings_from_json(self):
        settings = FindSettings()
        json = '''{
  "startpath": "~/src/xfind/",
  "in-ext": ["js","ts"],
  "out-dirpattern": "node_module",
  "out-filepattern": ["temp"],
  "findpattern": "Finder",
  "linesbefore": 2,
  "linesafter": 2,
  "debug": true,
  "allmatches": false,
  "includehidden": true
}'''
        self.findoptions.settings_from_json(json, settings)
        self.assertEqual(settings.startpath, '~/src/xfind/')
        for x in {'js', 'ts'}:
            self.assertIn(x, settings.in_extensions)
        self.assertEqual(list(settings.findpatterns)[0].pattern, 'Finder')
        self.assertEqual(list(settings.out_dirpatterns)[0].pattern, 'node_module')
        self.assertEqual(list(settings.out_filepatterns)[0].pattern, 'temp')
        self.assertEqual(settings.linesbefore, 2)
        self.assertEqual(settings.linesafter, 2)
        self.assertTrue(settings.debug)
        self.assertTrue(settings.verbose)
        self.assertTrue(settings.firstmatch)
        self.assertFalse(settings.excludehidden)


if __name__ == '__main__':
    unittest.main()
