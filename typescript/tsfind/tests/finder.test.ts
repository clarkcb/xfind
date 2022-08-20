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
     * isMatchingDir tests
     *************************************************************/
    it('testIsMatchingDir_SingleDot_True', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingDir('.')).toBeTruthy();
    });

    it('testIsMatchingDir_DoubleDot_True', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingDir('..')).toBeTruthy();
    });

    it('testIsMatchingDir_IsHidden_False', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingDir('.git')).toBeFalsy();
    });

    it('testIsMatchingDir_IsHiddenIncludeHidden_True', () => {
        const settings: FindSettings = getSettings();
        settings.excludeHidden = false;
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingDir('.git')).toBeTruthy();
    });

    it('testIsMatchingDir_NoPatterns_True', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingDir('/Users')).toBeTruthy();
    });

    it('testIsMatchingDir_MatchesInPattern_True', () => {
        const settings: FindSettings = getSettings();
        settings.addInDirPatterns('Find');
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingDir('CsFind')).toBeTruthy();
    });

    it('testIsMatchingDir_MatchesOutPattern_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutDirPatterns('Find');
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingDir('CsFind')).toBeFalsy();
    });

    it('testIsMatchingDir_DoesNotMatchInPattern_False', () => {
        const settings: FindSettings = getSettings();
        settings.addInDirPatterns('FindFiles');
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingDir('CsFind')).toBeFalsy();
    });

    it('testIsMatchingDir_DoesNotMatchOutPattern_True', () => {
        const settings: FindSettings = getSettings();
        settings.addOutDirPatterns('FindFiles');
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingDir('CsFind')).toBeTruthy();
    });

    /*************************************************************
     * isMatchingFile tests
     *************************************************************/
    it('testIsMatchingFile_NoExtensionsNoPatterns_True', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingFile('FileUtil.cs')).toBeTruthy();
    });

    it('testIsMatchingFile_MatchesInExtension_True', () => {
        const settings: FindSettings = getSettings();
        settings.addInExtensions('cs');
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingFile('FileUtil.cs')).toBeTruthy();
    });

    it('testIsMatchingFile_DoesNotMatchInExtension_False', () => {
        const settings: FindSettings = getSettings();
        settings.addInExtensions('java');
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingFile('FileUtil.cs')).toBeFalsy();
    });

    it('testIsMatchingFile_MatchesOutExtension_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutExtensions('cs');
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingFile('FileUtil.cs')).toBeFalsy();
    });

    it('testIsMatchingFile_DoesNotMatchOutExtension_True', () => {
        const settings: FindSettings = getSettings();
        settings.addOutExtensions('java');
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingFile('FileUtil.cs')).toBeTruthy();
    });

    it('testIsMatchingFile_MatchesInPattern_True', () => {
        const settings: FindSettings = getSettings();
        settings.addInFilePatterns('Find');
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingFile('Finder.cs')).toBeTruthy();
    });

    it('testIsMatchingFile_DoesNotMatchInPattern_False', () => {
        const settings: FindSettings = getSettings();
        settings.addInFilePatterns('Find');
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingFile('FileUtil.cs')).toBeFalsy();
    });

    it('testIsMatchingFile_MatchesOutPattern_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutFilePatterns('Find');
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingFile('Finder.cs')).toBeFalsy();
    });

    it('testIsMatchingFile_DoesNotMatchOutPattern_True', () => {
        const settings: FindSettings = getSettings();
        settings.addOutFilePatterns('Find');
        const finder: Finder = new Finder(settings);
        expect(finder.isMatchingFile('FileUtil.cs')).toBeTruthy();
    });

    /*************************************************************
     * IsArchiveFindFile tests
     *************************************************************/
    it('testIsMatchingArchiveFile_NoExtensionsNoPatterns_True', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeTruthy();
    });

    it('testIsMatchingArchiveFile_MatchesInExtension_True', () => {
        const settings: FindSettings = getSettings();
        settings.addInArchiveExtensions('zip');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeTruthy();
    });

    it('testIsMatchingArchiveFile_DoesNotMatchInExtension_False', () => {
        const settings: FindSettings = getSettings();
        settings.addInArchiveExtensions('gz');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeFalsy();
    });

    it('testIsMatchingArchiveFile_MatchesOutExtension_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutArchiveExtensions('zip');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeFalsy();
    });

    it('testIsMatchingArchiveFile_DoesNotMatchOutExtension_True', () => {
        const settings: FindSettings = getSettings();
        settings.addOutArchiveExtensions('gz');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeTruthy();
    });

    it('testIsMatchingArchiveFile_MatchesInPattern_True', () => {
        const settings: FindSettings = getSettings();
        settings.addInArchiveFilePatterns('arch');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeTruthy();
    });

    it('testIsMatchingArchiveFile_DoesNotMatchInPattern_False', () => {
        const settings: FindSettings = getSettings();
        settings.addInArchiveFilePatterns('archives');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeFalsy();
    });

    it('testIsMatchingArchiveFile_MatchesOutPattern_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutArchiveFilePatterns('arch');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeFalsy();
    });

    it('testIsMatchingArchiveFile_DoesNotMatchOutPattern_True', () => {
        const settings: FindSettings = getSettings();
        settings.addOutArchiveFilePatterns('archives');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeTruthy();
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
