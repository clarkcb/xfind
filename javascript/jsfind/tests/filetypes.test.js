/*
 * filetypes_test.js
 *
 * Some tests of filetypes.js
 */

const {FileType} = require('../src/filetype');
const {FileTypes} = require('../src/filetypes');

describe('testing filetypes', () => {
    it('testFileTypesArchiveFile', () => {
        const fileTypes = new FileTypes();
        const filename = 'archive.zip';
        fileTypes.isArchiveFile(filename).then((res) => {
            expect(res).toBeTruthy();
        });
        fileTypes.getFileType(filename).then((type) => {
            expect(type).toBe(FileType.ARCHIVE);
        });
    });

    it('testFileTypesAudioFile', () => {
        const fileTypes = new FileTypes();
        const filename = 'music.mp3';
        fileTypes.isAudioFile(filename).then((res) => {
            expect(res).toBeTruthy();
        });
        fileTypes.getFileType(filename).then((type) => {
            expect(type).toBe(FileType.AUDIO);
        });
    });

    it('testFileTypesBinaryFile', () => {
        const fileTypes = new FileTypes();
        const filename = 'binary.exe';
        fileTypes.isBinaryFile(filename).then((res) => {
            expect(res).toBeTruthy();
        });
        fileTypes.getFileType(filename).then((type) => {
            expect(type).toBe(FileType.BINARY);
        });
    });

    it('testFileTypesCodeFile', () => {
        const fileTypes = new FileTypes();
        const filename = 'code.js';
        fileTypes.isCodeFile(filename).then((res) => {
            expect(res).toBeTruthy();
        });
        fileTypes.getFileType(filename).then((type) => {
            expect(type).toBe(FileType.CODE);
        });
    });

    it('testFileTypesFontFile', () => {
        const fileTypes = new FileTypes();
        const filename = 'font.ttf';
        fileTypes.isFontFile(filename).then((res) => {
            expect(res).toBeTruthy();
        });
        fileTypes.getFileType(filename).then((type) => {
            expect(type).toBe(FileType.FONT);
        });
    });

    it('testFileTypesImageFile', () => {
        const fileTypes = new FileTypes();
        const filename = 'image.png';
        fileTypes.isImageFile(filename).then((res) => {
            expect(res).toBeTruthy();
        });
        fileTypes.getFileType(filename).then((type) => {
            expect(type).toBe(FileType.IMAGE);
        });
    });

    it('testFileTypesTextFile', () => {
        const fileTypes = new FileTypes();
        const filename = 'text.txt';
        fileTypes.isTextFile(filename).then((res) => {
            expect(res).toBeTruthy();
        });
        fileTypes.getFileType(filename).then((type) => {
            expect(type).toBe(FileType.TEXT);
        });
    });

    it('testFileTypesVideoFile', () => {
        const fileTypes = new FileTypes();
        const filename = 'movie.mp4';
        fileTypes.isVideoFile(filename).then((res) => {
            expect(res).toBeTruthy();
        });
        fileTypes.getFileType(filename).then((type) => {
            expect(type).toBe(FileType.VIDEO);
        });
    });

    it('testFileTypesXmlFile', () => {
        const fileTypes = new FileTypes();
        const filename = 'markup.xml';
        fileTypes.isXmlFile(filename).then((res) => {
            expect(res).toBeTruthy();
        });
        fileTypes.getFileType(filename).then((type) => {
            expect(type).toBe(FileType.XML);
        });
    });

    it('testFileTypesUnknownFile', () => {
        const fileTypes = new FileTypes();
        const filename = 'unknown.xyz';
        fileTypes.isUnknownFile(filename).then((res) => {
            expect(res).toBeTruthy();
        });
        fileTypes.getFileType(filename).then((type) => {
            expect(type).toBe(FileType.UNKNOWN);
        });
    });
});
