/*
 * filetypes.test.js
 *
 * Some tests of filetypes.js
 */

import { FileType } from '../src/filetype';
import { FileTypes } from '../src/filetypes';
import { FindConfig } from '../src/findconfig';

describe('testing filetypes', () => {
  it('testFileTypesArchiveFile', () => {
    const fileTypes = new FileTypes(new FindConfig());
    const filename = 'archive.zip';
    const res: boolean = fileTypes.isArchiveFile(filename);
    expect(res).toBeTruthy();

    const type: FileType = fileTypes.getFileType(filename);
    expect(type).toBe(FileType.Archive);
  });

  it('testFileTypesAudioFile', () => {
    const fileTypes = new FileTypes(new FindConfig());
    const filename = 'music.mp3';
    const res: boolean = fileTypes.isAudioFile(filename);
    expect(res).toBeTruthy();

    const type: FileType = fileTypes.getFileType(filename);
    expect(type).toBe(FileType.Audio);
  });

  it('testFileTypesBinaryFile', () => {
    const fileTypes = new FileTypes(new FindConfig());
    const filename = 'binary.exe';
    const res: boolean = fileTypes.isBinaryFile(filename);
    expect(res).toBeTruthy();

    const type: FileType = fileTypes.getFileType(filename);
    expect(type).toBe(FileType.Binary);
  });

  it('testFileTypesCodeFile', () => {
    const fileTypes = new FileTypes(new FindConfig());
    const filename = 'code.js';
    const res: boolean = fileTypes.isCodeFile(filename);
    expect(res).toBeTruthy();

    const type: FileType = fileTypes.getFileType(filename);
    expect(type).toBe(FileType.Code);
  });

  it('testFileTypesFontFile', () => {
    const fileTypes = new FileTypes(new FindConfig());
    const filename = 'font.ttf';
    const res: boolean = fileTypes.isFontFile(filename);
    expect(res).toBeTruthy();

    const type: FileType = fileTypes.getFileType(filename);
    expect(type).toBe(FileType.Font);
  });

  it('testFileTypesImageFile', () => {
    const fileTypes = new FileTypes(new FindConfig());
    const filename = 'image.png';
    const res: boolean = fileTypes.isImageFile(filename);
    expect(res).toBeTruthy();

    const type: FileType = fileTypes.getFileType(filename);
    expect(type).toBe(FileType.Image);
  });

  it('testFileTypesTextFile', () => {
    const fileTypes = new FileTypes(new FindConfig());
    const filename = 'text.txt';
    const res: boolean = fileTypes.isTextFile(filename);
    expect(res).toBeTruthy();

    const type: FileType = fileTypes.getFileType(filename);
    expect(type).toBe(FileType.Text);
  });

  it('testFileTypesVideoFile', () => {
    const fileTypes = new FileTypes(new FindConfig());
    const filename = 'movie.mp4';
    const res: boolean = fileTypes.isVideoFile(filename);
    expect(res).toBeTruthy();

    const type: FileType = fileTypes.getFileType(filename);
    expect(type).toBe(FileType.Video);
  });

  it('testFileTypesXmlFile', () => {
    const fileTypes = new FileTypes(new FindConfig());
    const filename = 'markup.xml';
    const res: boolean = fileTypes.isXmlFile(filename);
    expect(res).toBeTruthy();

    const type: FileType = fileTypes.getFileType(filename);
    expect(type).toBe(FileType.Xml);
  });

  it('testFileTypesUnknownFile', () => {
    const fileTypes = new FileTypes(new FindConfig());
    const filename = 'unknown.xyz';
    const res: boolean = fileTypes.isUnknownFile(filename);
    expect(res).toBeTruthy();

    const type: FileType = fileTypes.getFileType(filename);
    expect(type).toBe(FileType.Unknown);
  });
});
