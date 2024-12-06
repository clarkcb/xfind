/*
 * finder.test.js
 *
 * Some tests of finder.js
 */

import * as config from '../src/config';
import {Finder} from '../src/finder';
import {FindSettings} from '../src/findsettings';
import {FileType} from "../src/filetype";
import {FileResult} from "../src/fileresult";
import path from "path";

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
        settings.includeHidden = true;
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
     * isMatchingFileResult tests
     *************************************************************/
    it('testIsMatchingFileResult_NoExtensionsNoPatterns_True', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'FileUtil.cs', FileType.Code, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeTruthy();
    });

    it('testIsMatchingFileResult_MatchesInExtension_True', () => {
        const settings: FindSettings = getSettings();
        settings.addInExtensions('cs');
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'FileUtil.cs', FileType.Code, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeTruthy();
    });

    it('testIsMatchingFileResult_DoesNotMatchInExtension_False', () => {
        const settings: FindSettings = getSettings();
        settings.addInExtensions('java');
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'FileUtil.cs', FileType.Code, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeFalsy();
    });

    it('testIsMatchingFileResult_MatchesOutExtension_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutExtensions('cs');
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'FileUtil.cs', FileType.Code, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeFalsy();
    });

    it('testIsMatchingFileResult_DoesNotMatchOutExtension_True', () => {
        const settings: FindSettings = getSettings();
        settings.addOutExtensions('java');
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'FileUtil.cs', FileType.Code, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeTruthy();
    });

    it('testIsMatchingFileResult_MatchesInPattern_True', () => {
        const settings: FindSettings = getSettings();
        settings.addInFilePatterns('Find');
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'Finder.cs', FileType.Code, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeTruthy();
    });

    it('testIsMatchingFileResult_DoesNotMatchInPattern_False', () => {
        const settings: FindSettings = getSettings();
        settings.addInFilePatterns('Find');
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'FileUtil.cs', FileType.Code, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeFalsy();
    });

    it('testIsMatchingFileResult_MatchesOutPattern_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutFilePatterns('Find');
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'Finder.cs', FileType.Code, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeFalsy();
    });

    it('testIsMatchingFileResult_DoesNotMatchOutPattern_True', () => {
        const settings: FindSettings = getSettings();
        settings.addOutFilePatterns('Find');
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'FileUtil.cs', FileType.Code, 0, 0);
        expect(finder.isMatchingFileResult(fr)).toBeTruthy();
    });

    /*************************************************************
     * isMatchingArchiveFileResult tests
     *************************************************************/
    it('testIsMatchingArchiveFileResult_NoExtensionsNoPatterns_True', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.Archive, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeTruthy();
    });

    it('testIsMatchingArchiveFileResult_MatchesInExtension_True', () => {
        const settings: FindSettings = getSettings();
        settings.addInArchiveExtensions('zip');
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.Archive, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeTruthy();
    });

    it('testIsMatchingArchiveFileResult_DoesNotMatchInExtension_False', () => {
        const settings: FindSettings = getSettings();
        settings.addInArchiveExtensions('gz');
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.Archive, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeFalsy();
    });

    it('testIsMatchingArchiveFileResult_MatchesOutExtension_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutArchiveExtensions('zip');
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.Archive, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeFalsy();
    });

    it('testIsMatchingArchiveFileResult_DoesNotMatchOutExtension_True', () => {
        const settings: FindSettings = getSettings();
        settings.addOutArchiveExtensions('gz');
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.Archive, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeTruthy();
    });

    it('testIsMatchingArchiveFileResult_MatchesInPattern_True', () => {
        const settings: FindSettings = getSettings();
        settings.addInArchiveFilePatterns('arch');
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.Archive, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeTruthy();
    });

    it('testIsMatchingArchiveFileResult_DoesNotMatchInPattern_False', () => {
        const settings: FindSettings = getSettings();
        settings.addInArchiveFilePatterns('archives');
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.Archive, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeFalsy();
    });

    it('testIsMatchingArchiveFileResult_MatchesOutPattern_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutArchiveFilePatterns('arch');
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.Archive, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeFalsy();
    });

    it('testIsMatchingArchiveFileResult_DoesNotMatchOutPattern_True', () => {
        const settings: FindSettings = getSettings();
        settings.addOutArchiveFilePatterns('archives');
        const finder: Finder = new Finder(settings);
        const fr = new FileResult('.', 'archive.zip', FileType.Archive, 0, 0);
        expect(finder.isMatchingArchiveFileResult(fr)).toBeTruthy();
    });

    /*************************************************************
     * filterToFileResult tests
     *************************************************************/
    it('testFilterToFileResult_IsHidden_False', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        const file = '.gitignore';
        expect(finder.filterToFileResult(file)).toBeFalsy();
    });

    it('testFilterToFileResult_IsHiddenIncludeHidden_True', () => {
        const settings: FindSettings = getSettings();
        settings.includeHidden = true;
        const finder: Finder = new Finder(settings);
        const file = '.gitignore';
        expect(finder.filterToFileResult(file)).toBeTruthy();
    });

    it('testFilterToFileResult_ArchiveNoIncludeArchives_False', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.filterToFileResult(file)).toBeFalsy();
    });

    it('testFilterToFileResult_ArchiveIncludeArchives_True', () => {
        const settings: FindSettings = getSettings();
        settings.includeArchives = true;
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.filterToFileResult(file)).toBeTruthy();
    });

    it('testFilterToFileResult_IsArchiveFindFile_True', () => {
        const settings: FindSettings = getSettings();
        settings.includeArchives = true;
        settings.addInArchiveExtensions('zip');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.filterToFileResult(file)).toBeTruthy();
    });

    it('testFilterToFileResult_NotIsArchiveFindFile_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutExtensions('zip');
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.filterToFileResult(file)).toBeFalsy();
    });

    it('testFilterToFileResult_ArchiveFileArchivesOnly_True', () => {
        const settings: FindSettings = getSettings();
        settings.archivesOnly = true;
        const finder: Finder = new Finder(settings);
        const file = 'archive.zip';
        expect(finder.filterToFileResult(file)).toBeTruthy();
    });

    it('testFilterToFileResult_NoExtensionsNoPatterns_True', () => {
        const settings: FindSettings = getSettings();
        const finder: Finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.filterToFileResult(file)).toBeTruthy();
    });

    it('testFilterToFileResult_IsFindFile_True', () => {
        const settings: FindSettings = getSettings();
        settings.addInExtensions('cs');
        const finder: Finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.filterToFileResult(file)).toBeTruthy();
    });

    it('testFilterToFileResult_NotIsFindFile_False', () => {
        const settings: FindSettings = getSettings();
        settings.addOutExtensions('cs');
        const finder: Finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.filterToFileResult(file)).toBeFalsy();
    });

    it('testFilterToFileResult_NonArchiveFileArchivesOnly_False', () => {
        const settings: FindSettings = getSettings();
        settings.archivesOnly = true;
        const finder: Finder = new Finder(settings);
        const file = 'FileUtil.cs';
        expect(finder.filterToFileResult(file)).toBeFalsy();
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
            expect(fileResults.length < 3).toBeTruthy();
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
            expect(fileResults.length < 3).toBeTruthy();
        });
    });
});
