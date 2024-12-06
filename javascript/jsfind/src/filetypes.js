/*
 * filetypes.js
 *
 * identifies file types (archive, binary, text, unknown)
 */

const common = require('./common');
const config = require('./config');
const {FileType} = require('./filetype');
const {FileUtil} = require('./fileutil');

class FileTypes {
    constructor() {
        this.fileTypeExtMap = {};
        this.fileTypeNameMap = {};

        const json = FileUtil.getFileContentsSync(config.FILE_TYPES_JSON_PATH, 'utf-8');
        let obj = JSON.parse(json);
        if (Object.prototype.hasOwnProperty.call(obj, 'filetypes') && Array.isArray(obj.filetypes)) {
            obj.filetypes.forEach(ft => {
                let typename = ft.type;
                let extensions = ft.extensions;
                this.fileTypeExtMap[typename] = common.setFromArray(extensions);
                if (Object.prototype.hasOwnProperty.call(ft, 'names')) {
                    this.fileTypeNameMap[typename] = common.setFromArray(ft.names);
                } else {
                    this.fileTypeNameMap[typename] = [];
                }
            });
        } else throw new Error("Invalid filetypes file: " + config.FILE_TYPES_JSON_PATH);
        this.fileTypeExtMap.text = [].concat(this.fileTypeExtMap.text, this.fileTypeExtMap.code, this.fileTypeExtMap.xml);
        this.fileTypeNameMap.text = [].concat(this.fileTypeNameMap.text, this.fileTypeNameMap.code, this.fileTypeNameMap.xml);
    }

    getFileType(fileName) {
        // most specific first
        if (this.isCodeFile(fileName))
            return FileType.CODE;
        if (this.isArchiveFile(fileName))
            return FileType.ARCHIVE;
        if (this.isAudioFile(fileName))
            return FileType.AUDIO;
        if (this.isFontFile(fileName))
            return FileType.FONT;
        if (this.isImageFile(fileName))
            return FileType.IMAGE;
        if (this.isVideoFile(fileName))
            return FileType.VIDEO;
        // most general last
        if (this.isXmlFile(fileName))
            return FileType.XML;
        if (this.isTextFile(fileName))
            return FileType.TEXT;
        if (this.isBinaryFile(fileName))
            return FileType.BINARY;
        return FileType.UNKNOWN;
    }

    getFileTypeAsync(fileName, cb) {
        try {
            // most specific first
            if (this.isCodeFile(fileName))
                return cb(null, FileType.CODE);
            if (this.isArchiveFile(fileName))
                return cb(null, FileType.ARCHIVE);
            if (this.isAudioFile(fileName))
                return cb(null, FileType.AUDIO);
            if (this.isFontFile(fileName))
                return cb(null, FileType.FONT);
            if (this.isImageFile(fileName))
                return cb(null, FileType.IMAGE);
            if (this.isVideoFile(fileName))
                return cb(null, FileType.VIDEO);
            // most general last
            if (this.isXmlFile(fileName))
                return cb(null, FileType.XML);
            if (this.isTextFile(fileName))
                return cb(null, FileType.TEXT);
            if (this.isBinaryFile(fileName))
                return cb(null, FileType.BINARY);
        } catch (err) {
            return cb(err);
        }
        return cb(null, FileType.UNKNOWN);
    }

    isArchiveFile(fileName) {
        return this.fileTypeNameMap.archive.indexOf(fileName) > -1 ||
            this.fileTypeExtMap.archive.indexOf(FileUtil.getExtension(fileName)) > -1;
    }

    isAudioFile(fileName) {
        return this.fileTypeNameMap.audio.indexOf(fileName) > -1 ||
            this.fileTypeExtMap.audio.indexOf(FileUtil.getExtension(fileName)) > -1;
    }

    isBinaryFile(fileName) {
        return this.fileTypeNameMap.binary.indexOf(fileName) > -1 ||
            this.fileTypeExtMap.binary.indexOf(FileUtil.getExtension(fileName)) > -1;
    }

    isCodeFile(fileName) {
        return this.fileTypeNameMap.code.indexOf(fileName) > -1 ||
            this.fileTypeExtMap.code.indexOf(FileUtil.getExtension(fileName)) > -1;
    }

    isFontFile(fileName) {
        return this.fileTypeNameMap.font.indexOf(fileName) > -1 ||
            this.fileTypeExtMap.font.indexOf(FileUtil.getExtension(fileName)) > -1;
    }

    isImageFile(fileName) {
        return this.fileTypeNameMap.image.indexOf(fileName) > -1 ||
            this.fileTypeExtMap.image.indexOf(FileUtil.getExtension(fileName)) > -1;
    }

    isTextFile(fileName) {
        return this.fileTypeNameMap.text.indexOf(fileName) > -1 ||
            this.fileTypeExtMap.text.indexOf(FileUtil.getExtension(fileName)) > -1;
    }

    isVideoFile(fileName) {
        return this.fileTypeNameMap.video.indexOf(fileName) > -1 ||
            this.fileTypeExtMap.video.indexOf(FileUtil.getExtension(fileName)) > -1;
    }

    isXmlFile(fileName) {
        return this.fileTypeNameMap.xml.indexOf(fileName) > -1 ||
            this.fileTypeExtMap.xml.indexOf(FileUtil.getExtension(fileName)) > -1;
    }

    isUnknownFile(fileName) {
        return this.getFileType(fileName) === FileType.UNKNOWN;
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
