/*
 * fileresult.test.js
 *
 * Some tests of fileresult.js
 */

import {FileType} from '../src/filetype';
import {FileResult} from '../src/fileresult';

describe('testing fileresult', () => {
    it('testFileResultAbsPath', () => {
        const pathname = `${process.env.HOME}/src/xfind/javascript/jsfind/src`;
        const filename = 'fileresult.js';
        const fileresult = new FileResult(pathname, filename, FileType.Code);
        const expected = `${process.env.HOME}/src/xfind/javascript/jsfind/src/fileresult.js`;
        expect(fileresult.relativePath()).toEqual(expected);
    });

    it('testFileResultTildePath', () => {
        const pathname = '~/src/xfind/javascript/jsfind/src';
        const filename = 'fileresult.js';
        const fileresult = new FileResult(pathname, filename, FileType.Code);
        const expected = '~/src/xfind/javascript/jsfind/src/fileresult.js';
        expect(fileresult.relativePath()).toEqual(expected);
    });

    it('testFileResultRelPath1', () => {
        const pathname = '.';
        const filename = 'fileresult.js';
        const fileresult = new FileResult(pathname, filename, FileType.Code);
        const expected = './fileresult.js';
        expect(fileresult.relativePath()).toEqual(expected);
    });

    it('testFileResultRelPath2', () => {
        const pathname = './';
        const filename = 'fileresult.js';
        const fileresult = new FileResult(pathname, filename, FileType.Code);
        const expected = './fileresult.js';
        expect(fileresult.relativePath()).toEqual(expected);
    });

    it('testFileResultRelPath3', () => {
        const pathname = '..';
        const filename = 'fileresult.js';
        const fileresult = new FileResult(pathname, filename, FileType.Code);
        const expected = '../fileresult.js';
        expect(fileresult.relativePath()).toEqual(expected);
    });
});
