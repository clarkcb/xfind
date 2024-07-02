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

        const json = FileUtil.getFileContentsSync(config.FILETYPESJSONPATH, 'utf-8');
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
        } else throw new Error("Invalid filetypes file: " + config.FILETYPESJSONPATH);
        this.fileTypeExtMap.text = [].concat(this.fileTypeExtMap.text, this.fileTypeExtMap.code, this.fileTypeExtMap.xml);
        this.fileTypeNameMap.text = [].concat(this.fileTypeNameMap.text, this.fileTypeNameMap.code, this.fileTypeNameMap.xml);
    }

    getFileType(filename) {
        // most specific first
        if (this.isCodeFile(filename))
            return FileType.CODE;
        if (this.isArchiveFile(filename))
            return FileType.ARCHIVE;
        if (this.isAudioFile(filename))
            return FileType.AUDIO;
        if (this.isFontFile(filename))
            return FileType.FONT;
        if (this.isImageFile(filename))
            return FileType.IMAGE;
        if (this.isVideoFile(filename))
            return FileType.VIDEO;
        // most general last
        if (this.isXmlFile(filename))
            return FileType.XML;
        if (this.isTextFile(filename))
            return FileType.TEXT;
        if (this.isBinaryFile(filename))
            return FileType.BINARY;
        return FileType.UNKNOWN;
    }

    getFileTypeAsync(filename, cb) {
        try {
            // most specific first
            if (this.isCodeFile(filename))
                return cb(null, FileType.CODE);
            if (this.isArchiveFile(filename))
                return cb(null, FileType.ARCHIVE);
            if (this.isAudioFile(filename))
                return cb(null, FileType.AUDIO);
            if (this.isFontFile(filename))
                return cb(null, FileType.FONT);
            if (this.isImageFile(filename))
                return cb(null, FileType.IMAGE);
            if (this.isVideoFile(filename))
                return cb(null, FileType.VIDEO);
            // most general last
            if (this.isXmlFile(filename))
                return cb(null, FileType.XML);
            if (this.isTextFile(filename))
                return cb(null, FileType.TEXT);
            if (this.isBinaryFile(filename))
                return cb(null, FileType.BINARY);
        } catch (err) {
            return cb(err);
        }
        return cb(null, FileType.UNKNOWN);
    }

    isArchiveFile(filename) {
        return this.fileTypeNameMap.archive.indexOf(filename) > -1 ||
            this.fileTypeExtMap.archive.indexOf(FileUtil.getExtension(filename)) > -1;
    }

    isAudioFile(filename) {
        return this.fileTypeNameMap.audio.indexOf(filename) > -1 ||
            this.fileTypeExtMap.audio.indexOf(FileUtil.getExtension(filename)) > -1;
    }

    isBinaryFile(filename) {
        return this.fileTypeNameMap.binary.indexOf(filename) > -1 ||
            this.fileTypeExtMap.binary.indexOf(FileUtil.getExtension(filename)) > -1;
    }

    isCodeFile(filename) {
        return this.fileTypeNameMap.code.indexOf(filename) > -1 ||
            this.fileTypeExtMap.code.indexOf(FileUtil.getExtension(filename)) > -1;
    }

    isFontFile(filename) {
        return this.fileTypeNameMap.font.indexOf(filename) > -1 ||
            this.fileTypeExtMap.font.indexOf(FileUtil.getExtension(filename)) > -1;
    }

    isImageFile(filename) {
        return this.fileTypeNameMap.image.indexOf(filename) > -1 ||
            this.fileTypeExtMap.image.indexOf(FileUtil.getExtension(filename)) > -1;
    }

    isTextFile(filename) {
        return this.fileTypeNameMap.text.indexOf(filename) > -1 ||
            this.fileTypeExtMap.text.indexOf(FileUtil.getExtension(filename)) > -1;
    }

    isVideoFile(filename) {
        return this.fileTypeNameMap.video.indexOf(filename) > -1 ||
            this.fileTypeExtMap.video.indexOf(FileUtil.getExtension(filename)) > -1;
    }

    isXmlFile(filename) {
        return this.fileTypeNameMap.xml.indexOf(filename) > -1 ||
            this.fileTypeExtMap.xml.indexOf(FileUtil.getExtension(filename)) > -1;
    }

    isUnknownFile(filename) {
        return this.getFileType(filename) === FileType.UNKNOWN;
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
