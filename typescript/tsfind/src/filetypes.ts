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
        const fs = require('fs');

        let json = '';
        if (fs.existsSync(FileUtil.expandPath(config.FILETYPESJSONPATH))) {
            json = fs.readFileSync(FileUtil.expandPath(config.FILETYPESJSONPATH)).toString();
        } else {
            throw new Error('File not found: ' + config.FILETYPESJSONPATH);
        }

        const fileTypeExtMap: FileTypeMap = {};
        const fileTypeNameMap: FileTypeMap = {};

        const obj = JSON.parse(json);
        if (obj.hasOwnProperty('filetypes') && Array.isArray(obj['filetypes'])) {
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
        const uname: string = name.toUpperCase();
        if (uname === 'ARCHIVE')
            return FileType.Archive;
        if (uname === 'BINARY')
            return FileType.Binary;
        if (uname === 'CODE')
            return FileType.Code;
        if (uname === 'TEXT')
            return FileType.Text;
        if (uname === 'XML')
            return FileType.Xml;
        return FileType.Unknown;
    }

    public static toName(fileType: FileType): string {
        if (fileType === FileType.Archive)
            return 'ARCHIVE';
        if (fileType === FileType.Binary)
            return 'BINARY';
        if (fileType === FileType.Code)
            return 'CODE';
        if (fileType === FileType.Text)
            return 'TEXT';
        if (fileType === FileType.Xml)
            return 'XML';
        return 'UNKNOWN';
    }

    public static getFileType(filename: string): FileType {
        if (FileTypes.isCodeFile(filename))
            return FileType.Code;
        if (FileTypes.isXmlFile(filename))
            return FileType.Xml;
        if (FileTypes.isTextFile(filename))
            return FileType.Text;
        if (FileTypes.isBinaryFile(filename))
            return FileType.Binary;
        if (FileTypes.isArchiveFile(filename))
            return FileType.Archive;
        return FileType.Unknown;
    }

    public static getFileTypeAsync(filename: string, cb: (ft: FileType) => void): void {
        if (FileTypes.isCodeFile(filename))
            return cb(FileType.Code);
        if (FileTypes.isXmlFile(filename))
            return cb(FileType.Xml);
        if (FileTypes.isTextFile(filename))
            return cb(FileType.Text);
        if (FileTypes.isBinaryFile(filename))
            return cb(FileType.Binary);
        if (FileTypes.isArchiveFile(filename))
            return cb(FileType.Archive);
        cb(FileType.Unknown);
    }

    public static isArchiveFile(filename: string): boolean {
        return FileTypes.fileTypeExtMap['archive'].indexOf(FileUtil.getExtension(filename)) > -1
            || FileTypes.fileTypeNameMap['archive'].indexOf(filename) > -1;
    }

    public static isBinaryFile(filename: string): boolean {
        return FileTypes.fileTypeNameMap['binary'].indexOf(filename) > -1
            || FileTypes.fileTypeExtMap['binary'].indexOf(FileUtil.getExtension(filename)) > -1;
    }

    public static isCodeFile(filename: string): boolean {
        return FileTypes.fileTypeNameMap['code'].indexOf(filename) > -1
            || FileTypes.fileTypeExtMap['code'].indexOf(FileUtil.getExtension(filename)) > -1;
    }

    public static isTextFile(filename: string): boolean {
        return FileTypes.fileTypeNameMap['text'].indexOf(filename) > -1
            || FileTypes.fileTypeExtMap['text'].indexOf(FileUtil.getExtension(filename)) > -1;
    }

    public static isXmlFile(filename: string): boolean {
        return FileTypes.fileTypeNameMap['xml'].indexOf(filename) > -1
            || FileTypes.fileTypeExtMap['xml'].indexOf(FileUtil.getExtension(filename)) > -1;
    }

    public static isUnknownFile(filename: string): boolean {
        return FileTypes.getFileType(filename) === FileType.Unknown;
    }
}
