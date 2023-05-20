/*
 * findresult_test.js
 *
 * Some tests of findresult.js
 */

const {FileType} = require('../src/filetype');
const {FileResult} = require('../src/fileresult');

describe('testing fileresult', () => {
    it('testFileResultAbsPath', () => {
        const path = `${process.env.HOME}/src/xfind/javascript/jsfind/src`;
        const fileName = 'fileresult.js';
        const fileResult = new FileResult(path, fileName, FileType.CODE);
        const expected = `${process.env.HOME}/src/xfind/javascript/jsfind/src/fileresult.js`;
        expect(fileResult.relativePath()).toEqual(expected);
    });

    it('testFileResultRelPath1', () => {
        const path = '.';
        const fileName = 'fileresult.js';
        const fileResult = new FileResult(path, fileName, FileType.CODE);
        const expected = './fileresult.js';
        expect(fileResult.relativePath()).toEqual(expected);
    });

    it('testFileResultRelPath2', () => {
        const path = './';
        const fileName = 'fileresult.js';
        const fileResult = new FileResult(path, fileName, FileType.CODE);
        const expected = './fileresult.js';
        expect(fileResult.relativePath()).toEqual(expected);
    });

    it('testFileResultRelPath3', () => {
        const path = '..';
        const fileName = 'fileresult.js';
        const fileResult = new FileResult(path, fileName, FileType.CODE);
        const expected = '../fileresult.js';
        expect(fileResult.relativePath()).toEqual(expected);
    });
});
