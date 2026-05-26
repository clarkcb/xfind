/*
 * fileresult.test.js
 *
 * Some tests of fileresult.js
 */

import {FileType} from '../src/filetype';
import {FileResult} from '../src/fileresult';

describe('testing fileresult', () => {
    it('testFileResultAbsPath', () => {
        const filePath = `${process.env.HOME}/src/xfind/javascript/jsfind/src/fileresult.js`;
        const fileResult = new FileResult(filePath, FileType.Code, 0, 0);
        const expected = `${process.env.HOME}/src/xfind/javascript/jsfind/src/fileresult.js`;
        expect(fileResult.filePath).toEqual(expected);
    });

    it('testFileResultTildePath', () => {
        const filePath = '~/src/xfind/javascript/jsfind/src/fileresult.js';
        const fileResult = new FileResult(filePath, FileType.Code, 0, 0);
        const expected = '~/src/xfind/javascript/jsfind/src/fileresult.js';
        expect(fileResult.filePath).toEqual(expected);
    });

    it('testFileResultRelPath1', () => {
        const filePath = './fileresult.js';
        const fileResult = new FileResult(filePath, FileType.Code, 0, 0);
        const expected = './fileresult.js';
        expect(fileResult.filePath).toEqual(expected);
    });

    it('testFileResultRelPath2', () => {
        const filePath = './fileresult.js';
        const fileResult = new FileResult(filePath, FileType.Code, 0, 0);
        const expected = './fileresult.js';
        expect(fileResult.filePath).toEqual(expected);
    });

    it('testFileResultRelPath3', () => {
        const filePath = '../fileresult.js';
        const fileResult = new FileResult(filePath, FileType.Code, 0, 0);
        const expected = '../fileresult.js';
        expect(fileResult.filePath).toEqual(expected);
    });
});
