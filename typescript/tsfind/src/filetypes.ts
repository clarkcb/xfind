/*
 * filetypes.ts
 *
 * identifies file types (archive, binary, text, unknown)
 */

'use strict';

import * as config from './config';
import * as common from './common';
import {FileType} from './filetype';
import {FileUtil} from './fileutil';

interface FileTypeMap {
    [key: string]: string[]
}

export class FileTypes {
    private static fileTypeMaps: FileTypeMap[] = FileTypes.getFileTypeMaps();
    private static fileTypeExtMap: FileTypeMap = FileTypes.fileTypeMaps[0];
    private static fileTypeNameMap: FileTypeMap = FileTypes.fileTypeMaps[1];

    private static getFileTypeMaps(): FileTypeMap[] {
        const fileTypeExtMap: FileTypeMap = {};
        const fileTypeNameMap: FileTypeMap = {};
        const json = FileUtil.getFileContentsSync(config.FILETYPESJSONPATH);
        const obj = JSON.parse(json);
        if (Object.prototype.hasOwnProperty.call(obj, 'filetypes') && Array.isArray(obj['filetypes'])) {
            obj['filetypes'].forEach(ft => {
                const typename: string = ft['type'];
                const extensions: string[] = ft['extensions'];
                fileTypeExtMap[typename] = common.setFromArray(extensions);
                const names: string[] = ft['names'];
                fileTypeNameMap[typename] = common.setFromArray(names);
            });
        } else throw new Error("Invalid filetypes file: " + config.FILETYPESJSONPATH);

        fileTypeExtMap.text = fileTypeExtMap.text.concat(fileTypeExtMap.code, fileTypeExtMap.xml);
        fileTypeNameMap.text = fileTypeNameMap.text.concat(fileTypeNameMap.code, fileTypeNameMap.xml);
        fileTypeExtMap.findable = fileTypeExtMap.text.concat(fileTypeExtMap.binary, fileTypeExtMap.archive);

        return [fileTypeExtMap, fileTypeNameMap];
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

    public static getFileType(fileName: string): FileType {
        // most specific first
        if (FileTypes.isCodeFile(fileName))
            return FileType.Code;
        if (FileTypes.isArchiveFile(fileName))
            return FileType.Archive;
        if (FileTypes.isAudioFile(fileName))
            return FileType.Audio;
        if (FileTypes.isFontFile(fileName))
            return FileType.Font;
        if (FileTypes.isImageFile(fileName))
            return FileType.Image;
        if (FileTypes.isVideoFile(fileName))
            return FileType.Video;

        // most general last
        if (FileTypes.isXmlFile(fileName))
            return FileType.Xml;
        if (FileTypes.isTextFile(fileName))
            return FileType.Text;
        if (FileTypes.isBinaryFile(fileName))
            return FileType.Binary;
        return FileType.Unknown;
    }

    public static getFileTypeAsync(fileName: string, cb: (ft: FileType) => void): void {
        // most specific first
        if (FileTypes.isCodeFile(fileName))
            return cb(FileType.Code);
        if (FileTypes.isArchiveFile(fileName))
            return cb(FileType.Archive);
        if (FileTypes.isAudioFile(fileName))
            return cb(FileType.Audio);
        if (FileTypes.isFontFile(fileName))
            return cb(FileType.Font);
        if (FileTypes.isImageFile(fileName))
            return cb(FileType.Image);
        if (FileTypes.isVideoFile(fileName))
            return cb(FileType.Video);

        // most general last
        if (FileTypes.isXmlFile(fileName))
            return cb(FileType.Xml);
        if (FileTypes.isTextFile(fileName))
            return cb(FileType.Text);
        if (FileTypes.isBinaryFile(fileName))
            return cb(FileType.Binary);
        cb(FileType.Unknown);
    }

    public static isArchiveFile(fileName: string): boolean {
        return FileTypes.fileTypeExtMap['archive'].indexOf(FileUtil.getExtension(fileName)) > -1
            || FileTypes.fileTypeNameMap['archive'].indexOf(fileName) > -1;
    }

    public static isAudioFile(fileName: string): boolean {
        return FileTypes.fileTypeExtMap['audio'].indexOf(FileUtil.getExtension(fileName)) > -1
            || FileTypes.fileTypeNameMap['audio'].indexOf(fileName) > -1;
    }

    public static isBinaryFile(fileName: string): boolean {
        return FileTypes.fileTypeNameMap['binary'].indexOf(fileName) > -1
            || FileTypes.fileTypeExtMap['binary'].indexOf(FileUtil.getExtension(fileName)) > -1;
    }

    public static isCodeFile(fileName: string): boolean {
        return FileTypes.fileTypeNameMap['code'].indexOf(fileName) > -1
            || FileTypes.fileTypeExtMap['code'].indexOf(FileUtil.getExtension(fileName)) > -1;
    }

    public static isFontFile(fileName: string): boolean {
        return FileTypes.fileTypeNameMap['font'].indexOf(fileName) > -1
            || FileTypes.fileTypeExtMap['font'].indexOf(FileUtil.getExtension(fileName)) > -1;
    }

    public static isImageFile(fileName: string): boolean {
        return FileTypes.fileTypeNameMap['image'].indexOf(fileName) > -1
            || FileTypes.fileTypeExtMap['image'].indexOf(FileUtil.getExtension(fileName)) > -1;
    }

    public static isTextFile(fileName: string): boolean {
        return FileTypes.fileTypeNameMap['text'].indexOf(fileName) > -1
            || FileTypes.fileTypeExtMap['text'].indexOf(FileUtil.getExtension(fileName)) > -1;
    }

    public static isVideoFile(fileName: string): boolean {
        return FileTypes.fileTypeNameMap['video'].indexOf(fileName) > -1
            || FileTypes.fileTypeExtMap['video'].indexOf(FileUtil.getExtension(fileName)) > -1;
    }

    public static isXmlFile(fileName: string): boolean {
        return FileTypes.fileTypeNameMap['xml'].indexOf(fileName) > -1
            || FileTypes.fileTypeExtMap['xml'].indexOf(FileUtil.getExtension(fileName)) > -1;
    }

    public static isUnknownFile(fileName: string): boolean {
        return FileTypes.getFileType(fileName) === FileType.Unknown;
    }
}
