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
     * isFindDir tests
     *************************************************************/
    it('testisFindDir_SingleDot_True', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        expect(finder.isFindDir('.')).toBeTruthy();
    });

    it('testisFindDir_DoubleDot_True', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        expect(finder.isFindDir('..')).toBeTruthy();
    });

    it('testisFindDir_IsHidden_False', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        expect(finder.isFindDir('.git')).toBeFalsy();
    });

    it('testisFindDir_IsHiddenIncludeHidden_True', () => {
        const settings = getSettings();
        settings.excludeHidden = false;
        const finder = new Finder(settings);
        expect(finder.isFindDir('.git')).toBeTruthy();
    });

    it('testisFindDir_NoPatterns_True', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        expect(finder.isFindDir('/Users')).toBeTruthy();
    });

    it('testisFindDir_MatchesInPattern_True', () => {
        const settings = getSettings();
        settings.addInDirPatterns('Find');
        const finder = new Finder(settings);
        expect(finder.isFindDir('CsFind')).toBeTruthy();
    });

    it('testisFindDir_MatchesOutPattern_False', () => {
        const settings = getSettings();
        settings.addOutDirPatterns('Find');
        const finder = new Finder(settings);
        expect(finder.isFindDir('CsFind')).toBeFalsy();
    });

    it('testisFindDir_DoesNotMatchInPattern_False', () => {
        const settings = getSettings();
        settings.addInDirPatterns('FindFiles');
        const finder = new Finder(settings);
        expect(finder.isFindDir('CsFind')).toBeFalsy();
    });

    it('testisFindDir_DoesNotMatchOutPattern_True', () => {
        const settings = getSettings();
        settings.addOutDirPatterns('FindFiles');
        const finder = new Finder(settings);
        const dir = 'CsFind';
        expect(finder.isFindDir(dir)).toBeTruthy();
    });

    /*************************************************************
     * isFindFile tests
     *************************************************************/
    it('testIsFindFile_NoExtensionsNoPatterns_True', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.isFindFile(file)).toBeTruthy();
    });

    it('testIsFindFile_MatchesInExtension_True', () => {
        const settings = getSettings();
        settings.addInExtensions('cs');
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.isFindFile(file)).toBeTruthy();
    });

    it('testIsFindFile_DoesNotMatchInExtension_False', () => {
        const settings = getSettings();
        settings.addInExtensions('java');
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.isFindFile(file)).toBeFalsy();
    });

    it('testIsFindFile_MatchesOutExtension_False', () => {
        const settings = getSettings();
        settings.addOutExtensions('cs');
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.isFindFile(file)).toBeFalsy();
    });

    it('testIsFindFile_DoesNotMatchOutExtension_True', () => {
        const settings = getSettings();
        settings.addOutExtensions('java');
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.isFindFile(file)).toBeTruthy();
    });

    it('testIsFindFile_MatchesInPattern_True', () => {
        const settings = getSettings();
        settings.addInFilePatterns('Find');
        const finder = new Finder(settings);
        const file = 'Finder.cs';
        expect(finder.isFindFile(file)).toBeTruthy();
    });

    it('testIsFindFile_DoesNotMatchInPattern_False', () => {
        const settings = getSettings();
        settings.addInFilePatterns('Find');
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.isFindFile(file)).toBeFalsy();
    });

    it('testIsFindFile_MatchesOutPattern_False', () => {
        const settings = getSettings();
        settings.addOutFilePatterns('Find');
        const finder = new Finder(settings);
        const file = 'Finder.cs';
        expect(finder.isFindFile(file)).toBeFalsy();
    });

    it('testIsFindFile_DoesNotMatchOutPattern_True', () => {
        const settings = getSettings();
        settings.addOutFilePatterns('Find');
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.isFindFile(file)).toBeTruthy();
    });

    /*************************************************************
     * IsArchiveFindFile tests
     *************************************************************/
    it('testIsArchiveFindFile_NoExtensionsNoPatterns_True', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeTruthy();
    });

    it('testIsArchiveFindFile_MatchesInExtension_True', () => {
        const settings = getSettings();
        settings.addInArchiveExtensions('zip');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeTruthy();
    });

    it('testIsArchiveFindFile_DoesNotMatchInExtension_False', () => {
        const settings = getSettings();
        settings.addInArchiveExtensions('gz');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeFalsy();
    });

    it('testIsArchiveFindFile_MatchesOutExtension_False', () => {
        const settings = getSettings();
        settings.addOutArchiveExtensions('zip');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeFalsy();
    });

    it('testIsArchiveFindFile_DoesNotMatchOutExtension_True', () => {
        const settings = getSettings();
        settings.addOutArchiveExtensions('gz');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeTruthy();
    });

    it('testIsArchiveFindFile_MatchesInPattern_True', () => {
        const settings = getSettings();
        settings.addInArchiveFilePatterns('arch');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeTruthy();
    });

    it('testIsArchiveFindFile_DoesNotMatchInPattern_False', () => {
        const settings = getSettings();
        settings.addInArchiveFilePatterns('archives');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeFalsy();
    });

    it('testIsArchiveFindFile_MatchesOutPattern_False', () => {
        const settings = getSettings();
        settings.addOutArchiveFilePatterns('arch');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeFalsy();
    });

    it('testIsArchiveFindFile_DoesNotMatchOutPattern_True', () => {
        const settings = getSettings();
        settings.addOutArchiveFilePatterns('archives');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.isArchiveFindFile(file)).toBeTruthy();
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
