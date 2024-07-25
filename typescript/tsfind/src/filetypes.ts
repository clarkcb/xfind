/*
 * filetypes.ts
 *
 * identifies file types (archive, binary, text, unknown)
 */

'use strict';

import * as sqlite3 from 'sqlite3';
import * as config from './config';
import * as common from './common';
import {FileType} from './filetype';
import {FileUtil} from './fileutil';

interface ExtTypeMap {
    [key: string]: FileType
}

export class FileTypes {
    db: sqlite3.Database;
    ext_file_cache: ExtTypeMap = {};

    constructor() {
        this.db = new sqlite3.Database(config.XFINDDB, sqlite3.OPEN_READONLY, (err) => {
            if (err) {
                common.logError('Error connecting to the xfind database.');
            }
        });
    }

    async getFileTypeForSql(sql: string, params: string[]): Promise<FileType> {
        return new Promise((resolve, reject) => {
            this.db.get(sql, params, (err, row: any) => {
                if (err) {
                    common.logError(`Error getting file type for sql: ${sql}`);
                    reject(err);
                } else {
                    resolve(row ? row.file_type_id - 1 : FileType.Unknown);
                }
            });
        });
    }

    async getFileTypeForFilename(fileName: string): Promise<FileType> {
        const sql: string = 'SELECT file_type_id FROM file_name WHERE name = ?';
        return this.getFileTypeForSql(sql, [fileName]);
    }

    async getFileTypeForExtension(extension: string): Promise<FileType> {
        if (extension in this.ext_file_cache) {
            return this.ext_file_cache[extension];
        }
        const sql: string = 'SELECT file_type_id FROM file_extension WHERE extension = ?';
        const fileType: FileType = await this.getFileTypeForSql(sql, [extension]);
        this.ext_file_cache[extension] = fileType;
        return fileType;
    }

    public async getFileType(fileName: string): Promise<FileType> {
        const fileTypeForFilename = await this.getFileTypeForFilename(fileName);
        if (fileTypeForFilename !== FileType.Unknown) {
            return fileTypeForFilename;
        }
        const extension = FileUtil.getExtension(fileName);
        return await this.getFileTypeForExtension(extension);
    }

    public async isArchiveFile(fileName: string): Promise<boolean> {
        return await this.getFileType(fileName) === FileType.Archive;
    }

    public async isAudioFile(fileName: string): Promise<boolean> {
        return await this.getFileType(fileName) === FileType.Audio;

    }

    public async isBinaryFile(fileName: string): Promise<boolean> {
        return await this.getFileType(fileName) === FileType.Binary;
    }

    public async isCodeFile(fileName: string): Promise<boolean> {
        return await this.getFileType(fileName) === FileType.Code;
    }

    public async isFontFile(fileName: string): Promise<boolean> {
        return await this.getFileType(fileName) === FileType.Font;
    }

    public async isImageFile(fileName: string): Promise<boolean> {
        return await this.getFileType(fileName) === FileType.Image;
    }

    public async isTextFile(fileName: string): Promise<boolean> {
        const fileType = await this.getFileType(fileName);
        return fileType === FileType.Text || fileType === FileType.Code || fileType === FileType.Xml;
    }

    public async isVideoFile(fileName: string): Promise<boolean> {
        return await this.getFileType(fileName) === FileType.Video;
    }

    public async isXmlFile(fileName: string): Promise<boolean> {
        return await this.getFileType(fileName) === FileType.Xml;
    }

    public async isUnknownFile(fileName: string): Promise<boolean> {
        return await this.getFileType(fileName) === FileType.Unknown;
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
}
