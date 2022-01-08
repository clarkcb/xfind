# -*- coding: utf-8 -*-
################################################################################
#
# findsettings_test.py
#
# class FindSettingsTest: testing of FindSettings class
#
################################################################################
import os
import sys
import unittest

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

from pyfind import FindSettings


class FindSettingsTest(unittest.TestCase):
    def setUp(self):
        self.settings = FindSettings()

    def test_default_settings(self):
        # test the props
        self.assertFalse(self.settings.archivesonly)
        self.assertFalse(self.settings.debug)
        self.assertTrue(self.settings.excludehidden)
        self.assertFalse(self.settings.includearchives)
        self.assertFalse(self.settings.listdirs)
        self.assertFalse(self.settings.listfiles)
        self.assertFalse(self.settings.printusage)
        self.assertFalse(self.settings.printversion)
        self.assertTrue(self.settings.recursive)
        self.assertFalse(self.settings.verbose)
        # test the extension and pattern sets
        self.assertFalse(self.settings.in_archiveextensions)
        self.assertFalse(self.settings.in_archivefilepatterns)
        self.assertFalse(self.settings.in_dirpatterns)
        self.assertFalse(self.settings.in_filepatterns)
        self.assertFalse(self.settings.out_archiveextensions)
        self.assertFalse(self.settings.out_archivefilepatterns)
        self.assertFalse(self.settings.out_dirpatterns)
        self.assertFalse(self.settings.out_filepatterns)

    def test_set_properties(self):
        props = {
            'archivesonly': True,
            'debug': True,
            'excludehidden': False,
            'includearchives': True,
            'listdirs': True,
            'listfiles': True,
            'printusage': True,
            'printversion': True,
            'recursive': False,
            'verbose': True,
        }
        self.settings.set_properties(props)
        self.assertEqual(True, self.settings.archivesonly)
        self.assertEqual(True, self.settings.debug)
        self.assertEqual(False, self.settings.excludehidden)
        self.assertEqual(True, self.settings.includearchives)
        self.assertEqual(True, self.settings.listdirs)
        self.assertEqual(True, self.settings.listfiles)
        self.assertEqual(True, self.settings.printusage)
        self.assertEqual(True, self.settings.printversion)
        self.assertEqual(False, self.settings.recursive)
        self.assertEqual(True, self.settings.verbose)

    def test_add_single_extension(self):
        self.settings.add_exts('py', 'in_extensions')
        self.assertEqual(1, len(self.settings.in_extensions))
        self.assertIn('py', self.settings.in_extensions)

    def test_add_comma_delimited_extensions(self):
        self.settings.add_exts('py,rb,scala', 'in_extensions')
        self.assertEqual(3, len(self.settings.in_extensions))
        for x in {'py', 'rb', 'scala'}:
            self.assertIn(x, self.settings.in_extensions)

    def test_add_extensions_set(self):
        extensions_set = {'py', 'rb', 'scala'}
        self.settings.add_exts(extensions_set, 'in_extensions')
        self.assertEqual(3, len(self.settings.in_extensions))
        for x in extensions_set:
            self.assertIn(x, self.settings.in_extensions)


if __name__ == '__main__':
    unittest.main()
