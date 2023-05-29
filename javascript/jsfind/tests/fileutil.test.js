/*
 * fileutil_test.js
 *
 * Some tests of fileutil.js
 */

const FileUtil = require('../src/fileutil');

describe('testing fileutil', () => {
    /***************************************************************************
     * getExtension tests
     **************************************************************************/
    it('testGetTxtExtension', () => {
        const file = 'fileName.txt';
        expect(FileUtil.getExtension(file)).toEqual('txt');
    });

    it('testGetTxtExtensionAsync', () => {
        const file = 'fileName.txt';
        FileUtil.getExtensionAsync(file, (err, ext) => {
            expect(err).toBeNull();
            expect(ext).toBe('txt');
        });
    });

    it('testGetMissingExtension', () => {
        const file = 'fileName.';
        expect(FileUtil.getExtension(file)).toEqual('');
    });

    it('testGetMissingExtensionAsync', () => {
        const file = 'fileName.';
        FileUtil.getExtensionAsync(file, (err, ext) => {
            expect(err).toBeNull();
            expect(ext).toBe('');
        });
    });

    it('testGetNoExtension', () => {
        const file = 'filename';
        expect(FileUtil.getExtension(file)).toEqual('');
    });

    it('testGetNoExtensionAsync', () => {
        const file = 'filename';
        FileUtil.getExtensionAsync(file, (err, ext) => {
            expect(err).toBeNull();
            expect(ext).toEqual('');
        });
    });

    it('testGetHiddenTxtExtension', () => {
        const file = '.fileName.txt';
        expect(FileUtil.getExtension(file)).toEqual('txt');
    });

    it('testGetHiddenTxtExtensionAsync', () => {
        const file = '.fileName.txt';
        FileUtil.getExtensionAsync(file, (err, ext) => {
            expect(err).toBeNull();
            expect(ext).toBe('txt');
        });
    });

    it('testGetHiddenMissingExtension', () => {
        const file = 'fileName.';
        expect(FileUtil.getExtension(file)).toEqual('');
    });

    it('testGetHiddenMissingExtensionAsync', () => {
        const file = 'fileName.';
        FileUtil.getExtensionAsync(file, (err, ext) => {
            expect(err).toBeNull();
            expect(ext).toBe('');
        });
    });

    it('testGetHiddenNoExtension', () => {
        const file = 'filename';
        expect(FileUtil.getExtension(file)).toEqual('');
    });

    it('testGetHiddenNoExtensionAsync', () => {
        const file = 'filename';
        FileUtil.getExtensionAsync(file, (err, ext) => {
            expect(err).toBeNull();
            expect(ext).toBe('');
        });
    });

    it('testGetExtensionNonString', () => {
        const file = 200;
        try {
            // eslint-disable-next-line no-unused-vars
            const ext = FileUtil.getExtension(file);
        } catch (err) {
            expect(err).toBeDefined();
        }
    });

    it('testGetExtensionNonStringAsync', () => {
        const file = 200;
        FileUtil.getExtensionAsync(file, (err, ext) => {
            expect(err).toBeDefined();
            expect(ext).toBeUndefined();
        });
    });

    /***************************************************************************
     * getRelativePath tests
     **************************************************************************/
    it('testGetRelativePath', () => {
        const filePath = `${process.env.HOME}/filename.txt`;
        expect(FileUtil.getRelativePath(filePath, '.')).toEqual("./filename.txt");
    });

    /***************************************************************************
     * isDotDir tests
     **************************************************************************/
    it('testIsDotDirSingleDot', () => {
        const fileName = ".";
        expect(FileUtil.isDotDir(fileName)).toBeTruthy();
    });

    it('testIsDotDirDoubleDot', () => {
        const fileName = "..";
        expect(FileUtil.isDotDir(fileName)).toBeTruthy();
    });

    it('testIsDotDirNotDotDir', () => {
        const fileName = "~/path";
        expect(FileUtil.isDotDir(fileName)).toBeFalsy();
    });

    it('testIsDotDirPathWithDot', () => {
        const fileName = "./path";
        expect(FileUtil.isDotDir(fileName)).toBeFalsy();
    });

    it('testIsDotDirHiddenFile', () => {
        const fileName = ".gitignore";
        expect(FileUtil.isDotDir(fileName)).toBeFalsy();
    });

    /***************************************************************************
     * isHidden tests
     **************************************************************************/
    it('testIsHiddenSingleDot', () => {
        const fileName = ".";
        expect(FileUtil.isHidden(fileName)).toBeFalsy();
    });

    it('testIsHiddenDoubleDot', () => {
        const fileName = "..";
        expect(FileUtil.isHidden(fileName)).toBeFalsy();
    });

    it('testIsHiddenHiddenFile', () => {
        const fileName = ".gitignore";
        expect(FileUtil.isHidden(fileName)).toBeTruthy();
    });

    it('testIsHiddenNotHiddenFile', () => {
        const fileName = "file.txt";
        expect(FileUtil.isHidden(fileName)).toBeFalsy();
    });

    /***************************************************************************
     * expandPath tests
     **************************************************************************/
    it('testExpandPathPathWithTilde', () => {
        const filePath = "~/filename.txt";
        const expected = `${process.env.HOME}/filename.txt`;
        expect(FileUtil.expandPath(filePath) === expected).toBeTruthy();
    });

    it('testExpandPathPathNoTilde', () => {
        const filePath = "./fileName.txt";
        expect(FileUtil.expandPath(filePath) === filePath).toBeTruthy();
    });
});
