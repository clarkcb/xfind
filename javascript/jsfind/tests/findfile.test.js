/*
 * findresult_test.js
 *
 * Some tests of findresult.js
 */

const {FileType} = require('../src/filetype');
const FindFile = require('../src/findfile').FindFile;

describe('testing findfile', () => {
    it('testFindFileAbsPath', () => {
        const pathname = `${process.env.HOME}/src/xfind/javascript/jsfind/src`;
        const filename = 'findfile.js';
        const findfile = new FindFile(pathname, filename, FileType.CODE);
        const expected = `${process.env.HOME}/src/xfind/javascript/jsfind/src/findfile.js`;
        expect(findfile.relativePath()).toEqual(expected);
    });

    it('testFindFileRelPath1', () => {
        const pathname = '.';
        const filename = 'findfile.js';
        const findfile = new FindFile(pathname, filename, FileType.CODE);
        const expected = './findfile.js';
        expect(findfile.relativePath()).toEqual(expected);
    });

    it('testFindFileRelPath2', () => {
        const pathname = './';
        const filename = 'findfile.js';
        const findfile = new FindFile(pathname, filename, FileType.CODE);
        const expected = './findfile.js';
        expect(findfile.relativePath()).toEqual(expected);
    });

    it('testFindFileRelPath3', () => {
        const pathname = '..';
        const filename = 'findfile.js';
        const findfile = new FindFile(pathname, filename, FileType.CODE);
        const expected = '../findfile.js';
        expect(findfile.relativePath()).toEqual(expected);
    });
});
