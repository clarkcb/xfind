/*
 * finder_test.js
 *
 * Some tests of finder.js
 */

const Finder = require('../src/finder').Finder;
const FindSettings = require('../src/findsettings').FindSettings;

const getSettings = () => {
    let settings = new FindSettings();
    settings.paths.push('.');
    return settings;
};

describe('testing finder', () => {
    /*************************************************************
     * isMatchingDir tests
     *************************************************************/
    it('testIsMatchingDir_SingleDot_True', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        expect(finder.isMatchingDir('.')).toBeTruthy();
    });

    it('testIsMatchingDir_DoubleDot_True', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        expect(finder.isMatchingDir('..')).toBeTruthy();
    });

    it('testIsMatchingDir_IsHidden_False', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        expect(finder.isMatchingDir('.git')).toBeFalsy();
    });

    it('testIsMatchingDir_IsHiddenIncludeHidden_True', () => {
        const settings = getSettings();
        settings.excludeHidden = false;
        const finder = new Finder(settings);
        expect(finder.isMatchingDir('.git')).toBeTruthy();
    });

    it('testIsMatchingDir_NoPatterns_True', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        expect(finder.isMatchingDir('/Users')).toBeTruthy();
    });

    it('testIsMatchingDir_MatchesInPattern_True', () => {
        const settings = getSettings();
        settings.addInDirPatterns('Find');
        const finder = new Finder(settings);
        expect(finder.isMatchingDir('CsFind')).toBeTruthy();
    });

    it('testIsMatchingDir_MatchesOutPattern_False', () => {
        const settings = getSettings();
        settings.addOutDirPatterns('Find');
        const finder = new Finder(settings);
        expect(finder.isMatchingDir('CsFind')).toBeFalsy();
    });

    it('testIsMatchingDir_DoesNotMatchInPattern_False', () => {
        const settings = getSettings();
        settings.addInDirPatterns('FindFiles');
        const finder = new Finder(settings);
        expect(finder.isMatchingDir('CsFind')).toBeFalsy();
    });

    it('testIsMatchingDir_DoesNotMatchOutPattern_True', () => {
        const settings = getSettings();
        settings.addOutDirPatterns('FindFiles');
        const finder = new Finder(settings);
        const dir = 'CsFind';
        expect(finder.isMatchingDir(dir)).toBeTruthy();
    });

    /*************************************************************
     * isMatchingFile tests
     *************************************************************/
    it('testIsMatchingFile_NoExtensionsNoPatterns_True', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.isMatchingFile(file)).toBeTruthy();
    });

    it('testIsMatchingFile_MatchesInExtension_True', () => {
        const settings = getSettings();
        settings.addInExtensions('cs');
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.isMatchingFile(file)).toBeTruthy();
    });

    it('testIsMatchingFile_DoesNotMatchInExtension_False', () => {
        const settings = getSettings();
        settings.addInExtensions('java');
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.isMatchingFile(file)).toBeFalsy();
    });

    it('testIsMatchingFile_MatchesOutExtension_False', () => {
        const settings = getSettings();
        settings.addOutExtensions('cs');
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.isMatchingFile(file)).toBeFalsy();
    });

    it('testIsMatchingFile_DoesNotMatchOutExtension_True', () => {
        const settings = getSettings();
        settings.addOutExtensions('java');
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.isMatchingFile(file)).toBeTruthy();
    });

    it('testIsMatchingFile_MatchesInPattern_True', () => {
        const settings = getSettings();
        settings.addInFilePatterns('Find');
        const finder = new Finder(settings);
        const file = 'Finder.cs';
        expect(finder.isMatchingFile(file)).toBeTruthy();
    });

    it('testIsMatchingFile_DoesNotMatchInPattern_False', () => {
        const settings = getSettings();
        settings.addInFilePatterns('Find');
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.isMatchingFile(file)).toBeFalsy();
    });

    it('testIsMatchingFile_MatchesOutPattern_False', () => {
        const settings = getSettings();
        settings.addOutFilePatterns('Find');
        const finder = new Finder(settings);
        const file = 'Finder.cs';
        expect(finder.isMatchingFile(file)).toBeFalsy();
    });

    it('testIsMatchingFile_DoesNotMatchOutPattern_True', () => {
        const settings = getSettings();
        settings.addOutFilePatterns('Find');
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.isMatchingFile(file)).toBeTruthy();
    });

    /*************************************************************
     * isMatchingArchiveFile tests
     *************************************************************/
    it('testIsMatchingArchiveFile_NoExtensionsNoPatterns_True', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeTruthy();
    });

    it('testIsMatchingArchiveFile_MatchesInExtension_True', () => {
        const settings = getSettings();
        settings.addInArchiveExtensions('zip');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeTruthy();
    });

    it('testIsMatchingArchiveFile_DoesNotMatchInExtension_False', () => {
        const settings = getSettings();
        settings.addInArchiveExtensions('gz');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeFalsy();
    });

    it('testIsMatchingArchiveFile_MatchesOutExtension_False', () => {
        const settings = getSettings();
        settings.addOutArchiveExtensions('zip');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeFalsy();
    });

    it('testIsMatchingArchiveFile_DoesNotMatchOutExtension_True', () => {
        const settings = getSettings();
        settings.addOutArchiveExtensions('gz');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeTruthy();
    });

    it('testIsMatchingArchiveFile_MatchesInPattern_True', () => {
        const settings = getSettings();
        settings.addInArchiveFilePatterns('arch');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeTruthy();
    });

    it('testIsMatchingArchiveFile_DoesNotMatchInPattern_False', () => {
        const settings = getSettings();
        settings.addInArchiveFilePatterns('archives');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeFalsy();
    });

    it('testIsMatchingArchiveFile_MatchesOutPattern_False', () => {
        const settings = getSettings();
        settings.addOutArchiveFilePatterns('arch');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeFalsy();
    });

    it('testIsMatchingArchiveFile_DoesNotMatchOutPattern_True', () => {
        const settings = getSettings();
        settings.addOutArchiveFilePatterns('archives');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isMatchingArchiveFile(file)).toBeTruthy();
    });

    /*************************************************************
     * filterFile tests
     *************************************************************/
    it('testFilterFile_IsHidden_False', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        const file = '.gitignore';
        expect(finder.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_IsHiddenIncludeHidden_True', () => {
        const settings = getSettings();
        settings.excludeHidden = false;
        const finder = new Finder(settings);
        const file = '.gitignore';
        expect(finder.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_ArchiveNoFindArchives_False', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_ArchiveFindArchives_True', () => {
        const settings = getSettings();
        settings.findArchives = true;
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_IsArchiveFindFile_True', () => {
        const settings = getSettings();
        settings.findArchives = true;
        settings.addInArchiveExtensions('zip');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_NotIsArchiveFindFile_False', () => {
        const settings = getSettings();
        settings.addOutExtensions('zip');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_ArchiveFileArchivesOnly_True', () => {
        const settings = getSettings();
        settings.archivesOnly = true;
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_NoExtensionsNoPatterns_True', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_IsFindFile_True', () => {
        const settings = getSettings();
        settings.addInExtensions('cs');
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.filterFile(file)).toBeTruthy();
    });

    it('testFilterFile_NotIsFindFile_False', () => {
        const settings = getSettings();
        settings.addOutExtensions('cs');
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.filterFile(file)).toBeFalsy();
    });

    it('testFilterFile_NonArchiveFileArchivesOnly_False', () => {
        const settings = getSettings();
        settings.archivesOnly = true;
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.filterFile(file)).toBeFalsy();
    });
});
