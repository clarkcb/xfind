/*
 * filetypes.test.js
 *
 * Some tests of filetypes.js
 */

import {FileType} from '../src/filetype';
import {FileTypes} from '../src/filetypes';

describe('testing filetypes', () => {
    it('testFileTypesArchiveFile', () => {
        const fileTypes = new FileTypes();
        const fileName = 'archive.zip';
        fileTypes.isArchiveFile(fileName).then((res: boolean) => {
            expect(res).toBeTruthy();
        });
        fileTypes.getFileType(fileName).then((type: FileType) => {
            expect(type).toBe(FileType.Archive);
        });
    });

    it('testFileTypesAudioFile', () => {
        const fileTypes = new FileTypes();
        const fileName = 'music.mp3';
        fileTypes.isAudioFile(fileName).then((res: boolean) => {
            expect(res).toBeTruthy();
        });

        fileTypes.getFileType(fileName).then((type: FileType) => {
            expect(type).toBe(FileType.Audio);
        });
    });

    it('testFileTypesBinaryFile', () => {
        const fileTypes = new FileTypes();
        const fileName = 'binary.exe';
        fileTypes.isBinaryFile(fileName).then((res: boolean) => {
            expect(res).toBeTruthy();
        });

        fileTypes.getFileType(fileName).then((type: FileType) => {
            expect(type).toBe(FileType.Binary);
        });
    });

    it('testFileTypesCodeFile', () => {
        const fileTypes = new FileTypes();
        const fileName = 'code.js';
        fileTypes.isCodeFile(fileName).then((res: boolean) => {
            expect(res).toBeTruthy();
        });

        fileTypes.getFileType(fileName).then((type: FileType) => {
            expect(type).toBe(FileType.Code);
        });
    });

    it('testFileTypesFontFile', () => {
        const fileTypes = new FileTypes();
        const fileName = 'font.ttf';
        fileTypes.isFontFile(fileName).then((res: boolean) => {
            expect(res).toBeTruthy();
        });

        fileTypes.getFileType(fileName).then((type: FileType) => {
            expect(type).toBe(FileType.Font);
        });
    });

    it('testFileTypesImageFile', () => {
        const fileTypes = new FileTypes();
        const fileName = 'image.png';
        fileTypes.isImageFile(fileName).then((res: boolean) => {
            expect(res).toBeTruthy();
        });

        fileTypes.getFileType(fileName).then((type: FileType) => {
            expect(type).toBe(FileType.Image);
        });
    });

    it('testFileTypesTextFile', () => {
        const fileTypes = new FileTypes();
        const fileName = 'text.txt';
        fileTypes.isTextFile(fileName).then((res: boolean) => {
            expect(res).toBeTruthy();
        });

        fileTypes.getFileType(fileName).then((type: FileType) => {
            expect(type).toBe(FileType.Text);
        });
    });

    it('testFileTypesVideoFile', () => {
        const fileTypes = new FileTypes();
        const fileName = 'movie.mp4';
        fileTypes.isVideoFile(fileName).then((res: boolean) => {
            expect(res).toBeTruthy();
        });

        fileTypes.getFileType(fileName).then((type: FileType) => {
            expect(type).toBe(FileType.Video);
        });
    });

    it('testFileTypesXmlFile', () => {
        const fileTypes = new FileTypes();
        const fileName = 'markup.xml';
        fileTypes.isXmlFile(fileName).then((res: boolean) => {
            expect(res).toBeTruthy();
        });

        fileTypes.getFileType(fileName).then((type: FileType) => {
            expect(type).toBe(FileType.Xml);
        });
    });

    it('testFileTypesUnknownFile', () => {
        const fileTypes = new FileTypes();
        const fileName = 'unknown.xyz';
        fileTypes.isUnknownFile(fileName).then((res: boolean) => {
            expect(res).toBeTruthy();
        });

        fileTypes.getFileType(fileName).then((type: FileType) => {
            expect(type).toBe(FileType.Unknown);
        });
    });
});
