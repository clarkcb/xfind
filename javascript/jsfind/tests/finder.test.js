/*
 * finder_test.js
 *
 * Some tests of finder.js
 */

const path = require('path');

const config = require('../src/config');
const {FileType} = require("../src/filetype");
const {Finder} = require('../src/finder');
const {FindSettings} = require('../src/findsettings');
const { FileResult } = require('../src/fileresult')
// const expect = require("expect");

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
        settings.includeHidden = true;
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
     * isMatchingFileResult tests
     *************************************************************/
    it('testIsMatchingFileResult_NoExtensionsNoPatterns_True', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'FileUtil.cs', FileType.CODE, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeTruthy();
    });

    it('testIsMatchingFileResult_MatchesInExtension_True', () => {
        const settings = getSettings();
        settings.addInExtensions('cs');
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'FileUtil.cs', FileType.CODE, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeTruthy();
    });

    it('testIsMatchingFileResult_DoesNotMatchInExtension_False', () => {
        const settings = getSettings();
        settings.addInExtensions('java');
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'FileUtil.cs', FileType.CODE, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeFalsy();
    });

    it('testIsMatchingFileResult_MatchesOutExtension_False', () => {
        const settings = getSettings();
        settings.addOutExtensions('cs');
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'FileUtil.cs', FileType.CODE, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeFalsy();
    });

    it('testIsMatchingFileResult_DoesNotMatchOutExtension_True', () => {
        const settings = getSettings();
        settings.addOutExtensions('java');
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'FileUtil.cs', FileType.CODE, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeTruthy();
    });

    it('testIsMatchingFileResult_MatchesInPattern_True', () => {
        const settings = getSettings();
        settings.addInFilePatterns('Find');
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'Finder.cs', FileType.CODE, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeTruthy();
    });

    it('testIsMatchingFileResult_DoesNotMatchInPattern_False', () => {
        const settings = getSettings();
        settings.addInFilePatterns('Find');
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'FileUtil.cs', FileType.CODE, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeFalsy();
    });

    it('testIsMatchingFileResult_MatchesOutPattern_False', () => {
        const settings = getSettings();
        settings.addOutFilePatterns('Find');
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'Finder.cs', FileType.CODE, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeFalsy();
    });

    it('testIsMatchingFileResult_DoesNotMatchOutPattern_True', () => {
        const settings = getSettings();
        settings.addOutFilePatterns('Find');
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'FileUtil.cs', FileType.CODE, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeTruthy();
    });

    /*************************************************************
     * testIsMatchingArchiveFileResult tests
     *************************************************************/
    it('testIsMatchingArchiveFileResult_NoExtensionsNoPatterns_True', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.ARCHIVE, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeTruthy();
    });

    it('testIsMatchingArchiveFileResult_MatchesInExtension_True', () => {
        const settings = getSettings();
        settings.addInArchiveExtensions('zip');
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.ARCHIVE, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeTruthy();
    });

    it('testIsMatchingArchiveFileResult_DoesNotMatchInExtension_False', () => {
        const settings = getSettings();
        settings.addInArchiveExtensions('gz');
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.ARCHIVE, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeFalsy();
    });

    it('testIsMatchingArchiveFileResult_MatchesOutExtension_False', () => {
        const settings = getSettings();
        settings.addOutArchiveExtensions('zip');
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.ARCHIVE, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeFalsy();
    });

    it('testIsMatchingArchiveFileResult_DoesNotMatchOutExtension_True', () => {
        const settings = getSettings();
        settings.addOutArchiveExtensions('gz');
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.ARCHIVE, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeTruthy();
    });

    it('testIsMatchingArchiveFileResult_MatchesInPattern_True', () => {
        const settings = getSettings();
        settings.addInArchiveFilePatterns('arch');
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.ARCHIVE, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeTruthy();
    });

    it('testIsMatchingArchiveFileResult_DoesNotMatchInPattern_False', () => {
        const settings = getSettings();
        settings.addInArchiveFilePatterns('archives');
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.ARCHIVE, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeFalsy();
    });

    it('testIsMatchingArchiveFileResult_MatchesOutPattern_False', () => {
        const settings = getSettings();
        settings.addOutArchiveFilePatterns('arch');
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.ARCHIVE, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeFalsy();
    });

    it('testIsMatchingArchiveFileResult_DoesNotMatchOutPattern_True', () => {
        const settings = getSettings();
        settings.addOutArchiveFilePatterns('archives');
        const finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.ARCHIVE, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeTruthy();
    });

    /*************************************************************
     * filterToFileResult tests
     *************************************************************/
    it('testFilterToFileResult_IsHidden_False', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        const file = '.gitignore';
        finder.filterToFileResult(file, null).then((result) => {
            expect(result).toBeFalsy();
        });
    });

    it('testFilterToFileResult_IsHiddenIncludeHidden_True', () => {
        const settings = getSettings();
        settings.includeHidden = true;
        const finder = new Finder(settings);
        const file = '.gitignore';
        finder.filterToFileResult(file).then((result) => {
            expect(result).toBeTruthy();
        });
    });

    it('testFilterToFileResult_ArchiveNoFindArchives_False', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        const file = 'archive.zip';
        finder.filterToFileResult(file).then((result) => {
            expect(result).toBeFalsy();
        });
    });

    it('testFilterToFileResult_ArchiveIncludeArchives_True', () => {
        const settings = getSettings();
        settings.includeArchives = true;
        const finder = new Finder(settings);
        const file = 'archive.zip';
        finder.filterToFileResult(file).then((result) => {
            expect(result).toBeTruthy();
        });
    });

    it('testFilterToFileResult_IsArchiveFindFile_True', () => {
        const settings = getSettings();
        settings.includeArchives = true;
        settings.addInArchiveExtensions('zip');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        finder.filterToFileResult(file).then((result) => {
            expect(result).toBeTruthy();
        });
    });

    it('testFilterToFileResult_NotIsArchiveFindFile_False', () => {
        const settings = getSettings();
        settings.addOutExtensions('zip');
        const finder = new Finder(settings);
        const file = 'archive.zip';
        finder.filterToFileResult(file).then((result) => {
            expect(result).toBeFalsy();
        });
    });

    it('testFilterToFileResult_ArchiveFileArchivesOnly_True', () => {
        const settings = getSettings();
        settings.archivesOnly = true;
        expect(settings.archivesOnly).toBeTruthy();
        expect(settings.includeArchives).toBeTruthy();
        const finder = new Finder(settings);
        const file = 'archive.zip';
        finder.filterToFileResult(file).then((result) => {
            expect(result).toBeTruthy();
        });
    });

    it('testFilterToFileResult_NoExtensionsNoPatterns_True', () => {
        const settings = getSettings();
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        finder.filterToFileResult(file).then((result) => {
            expect(result).toBeTruthy();
        });
    });

    it('testFilterToFileResult_IsFindFile_True', () => {
        const settings = getSettings();
        settings.addInExtensions('cs');
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        finder.filterToFileResult(file).then((result) => {
            expect(result).toBeTruthy();
        });
    });

    it('testFilterToFileResult_NotIsFindFile_False', () => {
        const settings = getSettings();
        settings.addOutExtensions('cs');
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        finder.filterToFileResult(file).then((result) => {
            expect(result).toBeFalsy();
        });
    });

    it('testFilterToFileResult_NonArchiveFileArchivesOnly_False', () => {
        const settings = getSettings();
        settings.archivesOnly = true;
        const finder = new Finder(settings);
        const file = 'FileUtil.cs';
        finder.filterToFileResult(file).then((result) => {
            expect(result).toBeFalsy();
        });
    });

    /*************************************************************
     * test filtering symlink files
     *************************************************************/
    it('test_default_no_symlinks', () => {
        let settings = new FindSettings();
        let binPath = path.join(config.XFIND_PATH, 'bin');
        settings.paths.push(binPath);
        const finder = new Finder(settings);
        finder.find().then(fileResults => {
            expect(fileResults.length < 4).toBeTruthy();
        });
    });

    it('test_follow_symlinks', () => {
        let settings = new FindSettings();
        let binPath = path.join(config.XFIND_PATH, 'bin');
        settings.paths.push(binPath);
        settings.followSymlinks = true;
        const finder = new Finder(settings);
        finder.find().then(fileResults => {
            expect(fileResults.length === 0 || fileResults.length > 2).toBeTruthy();
        });
    });

    it('test_no_follow_symlinks', () => {
        let settings = new FindSettings();
        let binPath = path.join(config.XFIND_PATH, 'bin');
        settings.paths.push(binPath);
        settings.followSymlinks = false;
        const finder = new Finder(settings);
        finder.find().then(fileResults => {
            expect(fileResults.length < 4).toBeTruthy();
        });
    });
});
