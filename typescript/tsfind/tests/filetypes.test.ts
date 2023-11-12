/*
 * filetypes.test.js
 *
 * Some tests of filetypes.js
 */

import {FileType} from '../src/filetype';
import {FileTypes} from '../src/filetypes';

describe('testing filetypes', () => {
    it('testFileTypesArchiveFile', () => {
        const filename = 'archive.zip';
        const res: boolean = FileTypes.isArchiveFile(filename);
        expect(res).toBeTruthy();

        const type: FileType = FileTypes.getFileType(filename);
        expect(type).toBe(FileType.Archive);
    });

    it('testFileTypesAudioFile', () => {
        const filename = 'music.mp3';
        const res: boolean = FileTypes.isAudioFile(filename);
        expect(res).toBeTruthy();

        const type: FileType = FileTypes.getFileType(filename);
        expect(type).toBe(FileType.Audio);
    });

    it('testFileTypesBinaryFile', () => {
        const filename = 'binary.exe';
        const res: boolean = FileTypes.isBinaryFile(filename);
        expect(res).toBeTruthy();

        const type: FileType = FileTypes.getFileType(filename);
        expect(type).toBe(FileType.Binary);
    });

    it('testFileTypesCodeFile', () => {
        const filename = 'code.js';
        const res: boolean = FileTypes.isCodeFile(filename);
        expect(res).toBeTruthy();

        const type: FileType = FileTypes.getFileType(filename);
        expect(type).toBe(FileType.Code);
    });

    it('testFileTypesFontFile', () => {
        const filename = 'font.ttf';
        const res: boolean = FileTypes.isFontFile(filename);
        expect(res).toBeTruthy();

        const type: FileType = FileTypes.getFileType(filename);
        expect(type).toBe(FileType.Font);
    });

    it('testFileTypesImageFile', () => {
        const filename = 'image.png';
        const res: boolean = FileTypes.isImageFile(filename);
        expect(res).toBeTruthy();

        const type: FileType = FileTypes.getFileType(filename);
        expect(type).toBe(FileType.Image);
    });

    it('testFileTypesTextFile', () => {
        const filename = 'text.txt';
        const res: boolean = FileTypes.isTextFile(filename);
        expect(res).toBeTruthy();

        const type: FileType = FileTypes.getFileType(filename);
        expect(type).toBe(FileType.Text);
    });

    it('testFileTypesVideoFile', () => {
        const filename = 'movie.mp4';
        const res: boolean = FileTypes.isVideoFile(filename);
        expect(res).toBeTruthy();

        const type: FileType = FileTypes.getFileType(filename);
        expect(type).toBe(FileType.Video);
    });

    it('testFileTypesXmlFile', () => {
        const filename = 'markup.xml';
        const res: boolean = FileTypes.isXmlFile(filename);
        expect(res).toBeTruthy();

        const type: FileType = FileTypes.getFileType(filename);
        expect(type).toBe(FileType.Xml);
    });

    it('testFileTypesUnknownFile', () => {
        const filename = 'unknown.xyz';
        const res: boolean = FileTypes.isUnknownFile(filename);
        expect(res).toBeTruthy();

        const type: FileType = FileTypes.getFileType(filename);
        expect(type).toBe(FileType.Unknown);
    });
});
