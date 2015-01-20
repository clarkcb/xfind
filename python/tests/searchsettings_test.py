# -*- coding: utf-8 -*-
################################################################################
#
# searchsettings_test.py
#
# class SearchSettingsTest: testing of SearchSettings class
#
################################################################################
import re
import sys
from unittest import TestCase

from pysearch.searchsettings import SearchSettings

sys.path.insert(0, '/Users/cary/src/git/xsearch/python')

class SearchSettingsTest(TestCase):
    def setUp(self):
        self.settings = SearchSettings()

    def test_default_settings(self):
        # test the props
        self.assertEqual(self.settings.archivesonly, False)
        self.assertEqual(self.settings.debug, False)
        self.assertEqual(self.settings.dotiming, False)
        self.assertEqual(self.settings.firstmatch, False)
        self.assertEqual(self.settings.excludehidden, True)
        self.assertEqual(self.settings.linesafter, 0)
        self.assertEqual(self.settings.linesbefore, 0)
        self.assertEqual(self.settings.listdirs, False)
        self.assertEqual(self.settings.listfiles, False)
        self.assertEqual(self.settings.listlines, False)
        self.assertEqual(self.settings.maxlinelength, 150)
        self.assertEqual(self.settings.multilinesearch, False)
        self.assertEqual(self.settings.printresults, True)
        self.assertEqual(self.settings.printusage, False)
        self.assertEqual(self.settings.printversion, False)
        self.assertEqual(self.settings.recursive, True)
        self.assertEqual(self.settings.searcharchives, False)
        self.assertEqual(self.settings.uniquelines, False)
        self.assertEqual(self.settings.verbose, False)
        # test the extensino and pattern sets
        self.assertFalse(self.settings.in_archiveextensions)
        self.assertFalse(self.settings.in_archivefilepatterns)
        self.assertFalse(self.settings.in_dirpatterns)
        self.assertFalse(self.settings.in_filepatterns)
        self.assertFalse(self.settings.in_linesafterpatterns)
        self.assertFalse(self.settings.in_linesbeforepatterns)
        self.assertFalse(self.settings.linesaftertopatterns)
        self.assertFalse(self.settings.linesafteruntilpatterns)
        self.assertFalse(self.settings.out_archiveextensions)
        self.assertFalse(self.settings.out_archivefilepatterns)
        self.assertFalse(self.settings.out_dirpatterns)
        self.assertFalse(self.settings.out_filepatterns)
        self.assertFalse(self.settings.out_linesafterpatterns)
        self.assertFalse(self.settings.out_linesbeforepatterns)
        self.assertFalse(self.settings.searchpatterns)

    def test_set_properties(self):
        props = {
            'archivesonly': True,
            'debug': True,
            'dotiming': True,
            'firstmatch': True,
            'excludehidden': False,
            'linesafter': 5,
            'linesbefore': 5,
            'listdirs': True,
            'listfiles': True,
            'listlines': True,
            'maxlinelength': 155,
            'multilinesearch': True,
            'printresults': False,
            'printusage': True,
            'printversion': True,
            'recursive': False,
            'searcharchives': True,
            'uniquelines': True,
            'verbose': True,
        }
        self.settings.set_properties(props)
        self.assertEqual(self.settings.archivesonly, True)
        self.assertEqual(self.settings.debug, True)
        self.assertEqual(self.settings.dotiming, True)
        self.assertEqual(self.settings.firstmatch, True)
        self.assertEqual(self.settings.excludehidden, False)
        self.assertEqual(self.settings.linesafter, 5)
        self.assertEqual(self.settings.linesbefore, 5)
        self.assertEqual(self.settings.listdirs, True)
        self.assertEqual(self.settings.listfiles, True)
        self.assertEqual(self.settings.listlines, True)
        self.assertEqual(self.settings.maxlinelength, 155)
        self.assertEqual(self.settings.multilinesearch, True)
        self.assertEqual(self.settings.printresults, False)
        self.assertEqual(self.settings.printusage, True)
        self.assertEqual(self.settings.printversion, True)
        self.assertEqual(self.settings.recursive, False)
        self.assertEqual(self.settings.searcharchives, True)
        self.assertEqual(self.settings.uniquelines, True)
        self.assertEqual(self.settings.verbose, True)

    def test_add_single_extension(self):
        self.settings.add_comma_delimited_exts('py', 'in_extensions')
        self.assertIn('py', self.settings.in_extensions)

    def test_add_comma_delimited_extensions(self):
        self.settings.add_comma_delimited_exts('py,rb,scala', 'in_extensions')
        for x in set(['py', 'rb', 'scala']):
            self.assertIn(x, self.settings.in_extensions)

    def test_add_pattern(self):
        p = 'Search'
        self.settings.add_pattern(p, 'searchpatterns')
        self.assertEquals(list(self.settings.searchpatterns)[0].pattern, p)

if __name__ == '__main__':
    unittest.main()
