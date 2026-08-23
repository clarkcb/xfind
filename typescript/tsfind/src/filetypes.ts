/*
 * filetypes.ts
 *
 * identifies file types (archive, binary, text, unknown)
 */

'use strict';

import * as common from './common';
import { FileType } from './filetype';
import { FileUtil } from './fileutil';
import { FindConfig } from './findconfig';

interface FileTypeMap {
  [key: string]: string[];
}

export class FileTypes {
  private fileTypeExtMap: FileTypeMap;
  private fileTypeNameMap: FileTypeMap;

  constructor(config: FindConfig) {
    this.fileTypeExtMap = {};
    this.fileTypeNameMap = {};

    const json = FileUtil.getFileContentsSync(config.fileTypesPath, 'utf-8');
    let obj = JSON.parse(json);
    if (Object.prototype.hasOwnProperty.call(obj, 'filetypes') && Array.isArray(obj.filetypes)) {
      obj['filetypes'].forEach(ft => {
        let typename = ft.type;
        let extensions = ft.extensions;
        this.fileTypeExtMap[typename] = common.setFromArray(extensions);
        if (Object.prototype.hasOwnProperty.call(ft, 'names')) {
          this.fileTypeNameMap[typename] = common.setFromArray(ft.names);
        } else {
          this.fileTypeNameMap[typename] = [];
        }
      });
    } else throw new Error("Invalid filetypes file: " + config.fileTypesPath);
    this.fileTypeExtMap.text = this.fileTypeExtMap.text.concat(this.fileTypeExtMap.code, this.fileTypeExtMap.xml);
    this.fileTypeNameMap.text = this.fileTypeNameMap.text.concat(this.fileTypeNameMap.code, this.fileTypeNameMap.xml);
    this.fileTypeExtMap.findable = this.fileTypeExtMap.text.concat(this.fileTypeExtMap.binary, this.fileTypeExtMap.archive);
  }

  public getFileType(fileName: string): FileType {
    // most specific first
    if (this.isCodeFile(fileName))
      return FileType.Code;
    if (this.isArchiveFile(fileName))
      return FileType.Archive;
    if (this.isAudioFile(fileName))
      return FileType.Audio;
    if (this.isFontFile(fileName))
      return FileType.Font;
    if (this.isImageFile(fileName))
      return FileType.Image;
    if (this.isVideoFile(fileName))
      return FileType.Video;

    // most general last
    if (this.isXmlFile(fileName))
      return FileType.Xml;
    if (this.isTextFile(fileName))
      return FileType.Text;
    if (this.isBinaryFile(fileName))
      return FileType.Binary;
    return FileType.Unknown;
  }

  public getFileTypeAsync(fileName: string, cb: (ft: FileType) => void): void {
    // most specific first
    if (this.isCodeFile(fileName))
      return cb(FileType.Code);
    if (this.isArchiveFile(fileName))
      return cb(FileType.Archive);
    if (this.isAudioFile(fileName))
      return cb(FileType.Audio);
    if (this.isFontFile(fileName))
      return cb(FileType.Font);
    if (this.isImageFile(fileName))
      return cb(FileType.Image);
    if (this.isVideoFile(fileName))
      return cb(FileType.Video);

    // most general last
    if (this.isXmlFile(fileName))
      return cb(FileType.Xml);
    if (this.isTextFile(fileName))
      return cb(FileType.Text);
    if (this.isBinaryFile(fileName))
      return cb(FileType.Binary);
    cb(FileType.Unknown);
  }

  public isArchiveFile(fileName: string): boolean {
    return this.fileTypeExtMap['archive'].indexOf(FileUtil.getExtension(fileName)) > -1
        || this.fileTypeNameMap['archive'].indexOf(fileName) > -1;
  }

  public isAudioFile(fileName: string): boolean {
    return this.fileTypeExtMap['audio'].indexOf(FileUtil.getExtension(fileName)) > -1
        || this.fileTypeNameMap['audio'].indexOf(fileName) > -1;
  }

  public isBinaryFile(fileName: string): boolean {
    return this.fileTypeNameMap['binary'].indexOf(fileName) > -1
        || this.fileTypeExtMap['binary'].indexOf(FileUtil.getExtension(fileName)) > -1;
  }

  public isCodeFile(fileName: string): boolean {
    return this.fileTypeNameMap['code'].indexOf(fileName) > -1
        || this.fileTypeExtMap['code'].indexOf(FileUtil.getExtension(fileName)) > -1;
  }

  public isFontFile(fileName: string): boolean {
    return this.fileTypeNameMap['font'].indexOf(fileName) > -1
        || this.fileTypeExtMap['font'].indexOf(FileUtil.getExtension(fileName)) > -1;
  }

  public isImageFile(fileName: string): boolean {
    return this.fileTypeNameMap['image'].indexOf(fileName) > -1
        || this.fileTypeExtMap['image'].indexOf(FileUtil.getExtension(fileName)) > -1;
  }

  public isTextFile(fileName: string): boolean {
    return this.fileTypeNameMap['text'].indexOf(fileName) > -1
        || this.fileTypeExtMap['text'].indexOf(FileUtil.getExtension(fileName)) > -1;
  }

  public isVideoFile(fileName: string): boolean {
    return this.fileTypeNameMap['video'].indexOf(fileName) > -1
        || this.fileTypeExtMap['video'].indexOf(FileUtil.getExtension(fileName)) > -1;
  }

  public isXmlFile(fileName: string): boolean {
    return this.fileTypeNameMap['xml'].indexOf(fileName) > -1
        || this.fileTypeExtMap['xml'].indexOf(FileUtil.getExtension(fileName)) > -1;
  }

  public isUnknownFile(fileName: string): boolean {
    return this.getFileType(fileName) === FileType.Unknown;
  }

  public static fromName(name: string): FileType {
    switch (name.toLowerCase()) {
      case 'archive':
        return FileType.Archive;
      case 'audio':
        return FileType.Audio;
      case 'binary':
        return FileType.Binary;
      case 'code':
        return FileType.Code;
      case 'font':
        return FileType.Font;
      case 'image':
        return FileType.Image;
      case 'text':
        return FileType.Text;
      case 'video':
        return FileType.Video;
      case 'xml':
        return FileType.Xml;
      default:
        return FileType.Unknown;
    }
  }

  public static toName(fileType: FileType): string {
    switch (fileType) {
      case FileType.Archive:
        return 'archive';
      case FileType.Audio:
        return 'audio';
      case FileType.Binary:
        return 'binary';
      case FileType.Code:
        return 'code';
      case FileType.Font:
        return 'font';
      case FileType.Image:
        return 'image';
      case FileType.Text:
        return 'text';
      case FileType.Video:
        return 'video';
      case FileType.Xml:
        return 'xml';
      default:
        return 'unknown';
    }
  }

  public static fileTypesToString(name: string, fileTypes: FileType[]): string {
    let s = `${name}=[`;
    for (let i = 0; i < fileTypes.length; i++) {
      if (i > 0) s += ', ';
      s += FileTypes.toName(fileTypes[i]);
    }
    s += ']';
    return s;
  }
}
