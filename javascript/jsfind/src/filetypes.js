/*
 * filetypes.js
 *
 * identifies file types (archive, binary, text, unknown)
 */

const sqlite3 = require('sqlite3').verbose();
const common = require('./common');
const config = require('./config');
const {FileType} = require('./filetype');
const {FileUtil} = require('./fileutil');

class FileTypes {
    constructor() {
        this.db = new sqlite3.Database(config.XFINDDB, sqlite3.OPEN_READONLY, (err) => {
            if (err) {
                common.logError('Error connecting to the xfind database.');
            }
        });
        this.extTypeCache = {};
        this.nameTypeCache = {};
        this.nameCacheLoaded = false;
    }

    async getFileTypesForQueryAndParams(query, params) {
        return new Promise((resolve, reject) => {
            this.db.all(query, params, (err, rows) => {
                if (err) {
                    common.logError(`Error getting file type for query: ${query}`);
                    reject(err);
                } else {
                    let results = new Map(rows.map(r => [r.name, r.file_type_id - 1]));
                    resolve(results);
                }
            });
        });
    }

    async loadNameTypeCache() {
        let query = 'SELECT name, file_type_id FROM file_name';
        this.nameTypeCache = await this.getFileTypesForQueryAndParams(query, []);
    }

    async getFileTypeForQueryAndParams(query, params) {
        return new Promise((resolve, reject) => {
            this.db.get(query, params, (err, row) => {
                if (err) {
                    common.logError(`Error getting file type for query: ${query}`);
                    reject(err);
                } else {
                    resolve(row ? row.file_type_id - 1 : FileType.UNKNOWN);
                }
            });
        });
    }

    async getFileTypeForFilename(fileName) {
        if (!this.nameCacheLoaded) {
            await this.loadNameTypeCache();
            this.nameCacheLoaded = true;
        }
        if (this.nameTypeCache.has(fileName)) {
            return this.nameTypeCache.get(fileName);
        }
        // let sql = 'SELECT file_type_id FROM file_name WHERE name = ?';
        // return this.getFileTypeForQueryAndParams(sql, [fileName]);
        return FileType.UNKNOWN;
    }

    async getFileTypeForExtension(extension) {
        if (extension === '') {
            return FileType.UNKNOWN;
        }
        if (this.extTypeCache[extension]) {
            return this.extTypeCache[extension];
        }
        let sql = 'SELECT file_type_id FROM file_extension WHERE extension = ?';
        let fileType = await this.getFileTypeForQueryAndParams(sql, [extension]);
        this.extTypeCache[extension] = fileType;
        return fileType;
    }

    async getFileType(fileName) {
        let fileTypeForFilename = await this.getFileTypeForFilename(fileName);
        if (fileTypeForFilename !== FileType.UNKNOWN) {
            return fileTypeForFilename;
        }
        let extension = FileUtil.getExtension(fileName);
        return await this.getFileTypeForExtension(extension);
    }

    async isArchiveFile(fileName) {
        return await this.getFileType(fileName) === FileType.ARCHIVE;
    }

    async isAudioFile(fileName) {
        return await this.getFileType(fileName) === FileType.AUDIO;
    }

    async isBinaryFile(fileName) {
        return await this.getFileType(fileName) === FileType.BINARY;
    }

    async isCodeFile(fileName) {
        return await this.getFileType(fileName) === FileType.CODE;
    }

    async isFontFile(fileName) {
        return await this.getFileType(fileName) === FileType.FONT;
    }

    async isImageFile(fileName) {
        return await this.getFileType(fileName) === FileType.IMAGE;
    }

    async isTextFile(fileName) {
        const fileType = await this.getFileType(fileName);
        return fileType === FileType.TEXT || fileType === FileType.CODE || fileType === FileType.XML;
    }

    async isVideoFile(fileName) {
        return await this.getFileType(fileName) === FileType.VIDEO;
    }

    async isXmlFile(fileName) {
        return await this.getFileType(fileName) === FileType.XML;
    }

    async isUnknownFile(fileName) {
        return await this.getFileType(fileName) === FileType.UNKNOWN;
    }

    static fromName(name) {
        switch (name.toLowerCase()) {
            case 'archive':
                return FileType.ARCHIVE;
            case 'audio':
                return FileType.AUDIO;
            case 'binary':
                return FileType.BINARY;
            case 'code':
                return FileType.CODE;
            case 'font':
                return FileType.FONT;
            case 'image':
                return FileType.IMAGE;
            case 'text':
                return FileType.TEXT;
            case 'video':
                return FileType.VIDEO;
            case 'xml':
                return FileType.XML;
            default:
                return FileType.UNKNOWN;
        }
    }

    static toName(fileType) {
        switch (fileType) {
            case FileType.ARCHIVE:
                return 'archive';
            case FileType.AUDIO:
                return 'audio';
            case FileType.BINARY:
                return 'binary';
            case FileType.CODE:
                return 'code';
            case FileType.FONT:
                return 'font';
            case FileType.IMAGE:
                return 'image';
            case FileType.TEXT:
                return 'text';
            case FileType.VIDEO:
                return 'video';
            case FileType.XML:
                return 'xml';
            default:
                return 'unknown';
        }
    }

    static fileTypesToString(name, fileTypes) {
        if (fileTypes.length) {
            let s = `${name}=[`;
            for (let i=0; i < fileTypes.length; i++) {
                if (i > 0) s += ', ';
                s += FileTypes.toName(fileTypes[i]);
            }
            s += ']';
            return s;
        }
        return `${name}=[]`;
    }
}

exports.FileTypes = FileTypes;
