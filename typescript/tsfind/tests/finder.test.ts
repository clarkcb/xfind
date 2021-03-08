/*
 * finder.test.js
 *
 * Some tests of finder.js
 */

import {Finder} from '../src/finder';
import {FindSettings} from '../src/findsettings';

const getSettings = function() {
    const settings: FindSettings = new FindSettings();
    settings.paths.push('.');
    return settings;
};

describe('testing finder', () => {

    /*************************************************************
     * isFindDir tests
     *************************************************************/
    it('testisFindDir_SingleDot_True', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        expect(finder.isFindDir('.')).toBeTruthy();
    });

    it('testisFindDir_DoubleDot_True', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        expect(finder.isFindDir('..')).toBeTruthy();
    });

    it('testisFindDir_IsHidden_False', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        expect(finder.isFindDir('.git')).toBeFalsy();
    });

    it('testisFindDir_IsHiddenIncludeHidden_True', () => {
        const settings: FindSettings = getSettings();
        settings.excludeHidden = false;
        const finder: Finder = new Finder(settings);
        expect(finder.isFindDir('.git')).toBeTruthy();
    });

    it('testisFindDir_NoPatterns_True', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        expect(finder.isFindDir('/Users')).toBeTruthy();
    });

    it('testisFindDir_MatchesInPattern_True', () => {
        const settings: FindSettings = getSettings();
        settings.addInDirPatterns('Find');
        const finder: Finder = new Finder(settings);
        expect(finder.isFindDir('CsFind')).toBeTruthy();
    });

    it('testisFindDir_MatchesOutPattern_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutDirPatterns('Find');
        const finder: Finder = new Finder(settings);
        expect(finder.isFindDir('CsFind')).toBeFalsy();
    });

    it('testisFindDir_DoesNotMatchInPattern_False', () => {
        const settings: FindSettings = getSettings();
        settings.addInDirPatterns('FindFiles');
        const finder: Finder = new Finder(settings);
        expect(finder.isFindDir('CsFind')).toBeFalsy();
    });

    it('testisFindDir_DoesNotMatchOutPattern_True', () => {
        const settings: FindSettings = getSettings();
        settings.addOutDirPatterns('FindFiles');
        const finder: Finder = new Finder(settings);
        expect(finder.isFindDir('CsFind')).toBeTruthy();
    });

    /*************************************************************
     * isFindFile tests
     *************************************************************/
    it('testIsFindFile_NoExtensionsNoPatterns_True', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        expect(finder.isFindFile('FileUtil.cs')).toBeTruthy();
    });

    it('testIsFindFile_MatchesInExtension_True', () => {
        const settings: FindSettings = getSettings();
        settings.addInExtensions('cs');
        const finder: Finder = new Finder(settings);
        expect(finder.isFindFile('FileUtil.cs')).toBeTruthy();
    });

    it('testIsFindFile_DoesNotMatchInExtension_False', () => {
        const settings: FindSettings = getSettings();
        settings.addInExtensions('java');
        const finder: Finder = new Finder(settings);
        expect(finder.isFindFile('FileUtil.cs')).toBeFalsy();
    });

    it('testIsFindFile_MatchesOutExtension_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutExtensions('cs');
        const finder: Finder = new Finder(settings);
        expect(finder.isFindFile('FileUtil.cs')).toBeFalsy();
    });

    it('testIsFindFile_DoesNotMatchOutExtension_True', () => {
        const settings: FindSettings = getSettings();
        settings.addOutExtensions('java');
        const finder: Finder = new Finder(settings);
        expect(finder.isFindFile('FileUtil.cs')).toBeTruthy();
    });

    it('testIsFindFile_MatchesInPattern_True', () => {
        const settings: FindSettings = getSettings();
        settings.addInFilePatterns('Find');
        const finder: Finder = new Finder(settings);
        expect(finder.isFindFile('Finder.cs')).toBeTruthy();
    });

    it('testIsFindFile_DoesNotMatchInPattern_False', () => {
        const settings: FindSettings = getSettings();
        settings.addInFilePatterns('Find');
        const finder: Finder = new Finder(settings);
        expect(finder.isFindFile('FileUtil.cs')).toBeFalsy();
    });

    it('testIsFindFile_MatchesOutPattern_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutFilePatterns('Find');
        const finder: Finder = new Finder(settings);
        expect(finder.isFindFile('Finder.cs')).toBeFalsy();
    });

    it('testIsFindFile_DoesNotMatchOutPattern_True', () => {
        const settings: FindSettings = getSettings();
        settings.addOutFilePatterns('Find');
        const finder: Finder = new Finder(settings);
        expect(finder.isFindFile('FileUtil.cs')).toBeTruthy();
    });

    /*************************************************************
     * IsArchiveFindFile tests
     *************************************************************/
    it('testIsArchiveFindFile_NoExtensionsNoPatterns_True', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeTruthy();
    });

    it('testIsArchiveFindFile_MatchesInExtension_True', () => {
        const settings: FindSettings = getSettings();
        settings.addInArchiveExtensions('zip');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeTruthy();
    });

    it('testIsArchiveFindFile_DoesNotMatchInExtension_False', () => {
        const settings: FindSettings = getSettings();
        settings.addInArchiveExtensions('gz');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeFalsy();
    });

    it('testIsArchiveFindFile_MatchesOutExtension_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutArchiveExtensions('zip');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeFalsy();
    });

    it('testIsArchiveFindFile_DoesNotMatchOutExtension_True', () => {
        const settings: FindSettings = getSettings();
        settings.addOutArchiveExtensions('gz');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeTruthy();
    });

    it('testIsArchiveFindFile_MatchesInPattern_True', () => {
        const settings: FindSettings = getSettings();
        settings.addInArchiveFilePatterns('arch');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeTruthy();
    });

    it('testIsArchiveFindFile_DoesNotMatchInPattern_False', () => {
        const settings: FindSettings = getSettings();
        settings.addInArchiveFilePatterns('archives');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeFalsy();
    });

    it('testIsArchiveFindFile_MatchesOutPattern_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutArchiveFilePatterns('arch');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeFalsy();
    });

    it('testIsArchiveFindFile_DoesNotMatchOutPattern_True', () => {
        const settings: FindSettings = getSettings();
        settings.addOutArchiveFilePatterns('archives');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeTruthy();
    });

    /*************************************************************
     * filterFile tests
     *************************************************************/
    it('testFilterFile_IsHidden_False', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        const file = '.gitignore';
        expect(finder.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_IsHiddenIncludeHidden_True', () => {
        const settings: FindSettings = getSettings();
        settings.excludeHidden = false;
        const finder: Finder = new Finder(settings);
        const file = '.gitignore';
        expect(finder.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_ArchiveNoIncludeArchives_False', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_ArchiveIncludeArchives_True', () => {
        const settings: FindSettings = getSettings();
        settings.includeArchives = true;
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_IsArchiveFindFile_True', () => {
        const settings: FindSettings = getSettings();
        settings.includeArchives = true;
        settings.addInArchiveExtensions('zip');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_NotIsArchiveFindFile_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutExtensions('zip');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_ArchiveFileArchivesOnly_True', () => {
        const settings: FindSettings = getSettings();
        settings.archivesOnly = true;
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_NoExtensionsNoPatterns_True', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_IsFindFile_True', () => {
        const settings: FindSettings = getSettings();
        settings.addInExtensions('cs');
        const finder: Finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_NotIsFindFile_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutExtensions('cs');
        const finder: Finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_NonArchiveFileArchivesOnly_False', () => {
        const settings: FindSettings = getSettings();
        settings.archivesOnly = true;
        const finder: Finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.filterFile(file)).toBeFalsy();
    });
});
