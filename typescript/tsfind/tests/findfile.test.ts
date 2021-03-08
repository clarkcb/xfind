/*
 * findfile.test.js
 *
 * Some tests of findfile.js
 */

import {FileType} from '../src/filetype';
import {FindFile} from '../src/findfile';

describe('testing findfile', () => {
    it('testFindFileAbsPath', () => {
        const pathname = `${process.env.HOME}/src/xfind/javascript/jsfind/src`;
        const filename = 'findfile.js';
        const findfile = new FindFile(pathname, filename, FileType.Code);
        const expected = `${process.env.HOME}/src/xfind/javascript/jsfind/src/findfile.js`;
        expect(findfile.relativePath()).toEqual(expected);
    });

    it('testFindFileTildePath', () => {
        const pathname = '~/src/xfind/javascript/jsfind/src';
        const filename = 'findfile.js';
        const findfile = new FindFile(pathname, filename, FileType.Code);
        const expected = '~/src/xfind/javascript/jsfind/src/findfile.js';
        expect(findfile.relativePath()).toEqual(expected);
    });

    it('testFindFileRelPath1', () => {
        const pathname = '.';
        const filename = 'findfile.js';
        const findfile = new FindFile(pathname, filename, FileType.Code);
        const expected = './findfile.js';
        expect(findfile.relativePath()).toEqual(expected);
    });

    it('testFindFileRelPath2', () => {
        const pathname = './';
        const filename = 'findfile.js';
        const findfile = new FindFile(pathname, filename, FileType.Code);
        const expected = './findfile.js';
        expect(findfile.relativePath()).toEqual(expected);
    });

    it('testFindFileRelPath3', () => {
        const pathname = '..';
        const filename = 'findfile.js';
        const findfile = new FindFile(pathname, filename, FileType.Code);
        const expected = '../findfile.js';
        expect(findfile.relativePath()).toEqual(expected);
    });
});
