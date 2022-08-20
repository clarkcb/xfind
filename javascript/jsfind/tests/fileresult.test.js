/*
 * findresult_test.js
 *
 * Some tests of findresult.js
 */

const {FileType} = require('../src/filetype');
const {FileResult} = require('../src/fileresult');

describe('testing fileresult', () => {
    it('testFileResultAbsPath', () => {
        const pathname = `${process.env.HOME}/src/xfind/javascript/jsfind/src`;
        const filename = 'fileresult.js';
        const fileresult = new FileResult(pathname, filename, FileType.CODE);
        const expected = `${process.env.HOME}/src/xfind/javascript/jsfind/src/fileresult.js`;
        expect(fileresult.relativePath()).toEqual(expected);
    });

    it('testFileResultRelPath1', () => {
        const pathname = '.';
        const filename = 'fileresult.js';
        const fileresult = new FileResult(pathname, filename, FileType.CODE);
        const expected = './fileresult.js';
        expect(fileresult.relativePath()).toEqual(expected);
    });

    it('testFileResultRelPath2', () => {
        const pathname = './';
        const filename = 'fileresult.js';
        const fileresult = new FileResult(pathname, filename, FileType.CODE);
        const expected = './fileresult.js';
        expect(fileresult.relativePath()).toEqual(expected);
    });

    it('testFileResultRelPath3', () => {
        const pathname = '..';
        const filename = 'fileresult.js';
        const fileresult = new FileResult(pathname, filename, FileType.CODE);
        const expected = '../fileresult.js';
        expect(fileresult.relativePath()).toEqual(expected);
    });
});
