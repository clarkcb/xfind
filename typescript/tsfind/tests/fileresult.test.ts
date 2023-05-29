/*
 * fileresult.test.js
 *
 * Some tests of fileresult.js
 */

import {FileType} from '../src/filetype';
import {FileResult} from '../src/fileresult';

describe('testing fileresult', () => {
    it('testFileResultAbsPath', () => {
        const path = `${process.env.HOME}/src/xfind/javascript/jsfind/src`;
        const fileName = 'fileresult.js';
        const fileResult = new FileResult(path, fileName, FileType.Code, null);
        const expected = `${process.env.HOME}/src/xfind/javascript/jsfind/src/fileresult.js`;
        expect(fileResult.relativePath()).toEqual(expected);
    });

    it('testFileResultTildePath', () => {
        const path = '~/src/xfind/javascript/jsfind/src';
        const fileName = 'fileresult.js';
        const fileResult = new FileResult(path, fileName, FileType.Code, null);
        const expected = '~/src/xfind/javascript/jsfind/src/fileresult.js';
        expect(fileResult.relativePath()).toEqual(expected);
    });

    it('testFileResultRelPath1', () => {
        const path = '.';
        const fileName = 'fileresult.js';
        const fileResult = new FileResult(path, fileName, FileType.Code, null);
        const expected = './fileresult.js';
        expect(fileResult.relativePath()).toEqual(expected);
    });

    it('testFileResultRelPath2', () => {
        const path = './';
        const fileName = 'fileresult.js';
        const fileResult = new FileResult(path, fileName, FileType.Code, null);
        const expected = './fileresult.js';
        expect(fileResult.relativePath()).toEqual(expected);
    });

    it('testFileResultRelPath3', () => {
        const path = '..';
        const fileName = 'fileresult.js';
        const fileResult = new FileResult(path, fileName, FileType.Code, null);
        const expected = '../fileresult.js';
        expect(fileResult.relativePath()).toEqual(expected);
    });
});
