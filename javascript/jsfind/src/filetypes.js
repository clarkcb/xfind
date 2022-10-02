/*
 * filetypes.js
 *
 * identifies file types (archive, binary, text, unknown)
 */

const common = require('./common');
const config = require('./config');
const {FileType} = require('./filetype');
const {expandPath, getExtension} = require('./fileutil');

class FileTypes {
    'use strict'

    constructor() {
        [this.fileTypeExtMap, this.fileTypeNameMap] = (() => {
            let fs = require('fs');

            let json = '';
            if (fs.existsSync(expandPath(config.FILETYPESJSONPATH))) {
                json = fs.readFileSync(expandPath(config.FILETYPESJSONPATH)).toString();
            } else {
                throw new Error('File not found: ' + config.FILETYPESJSONPATH);
            }

            let fileTypeExtMap = {};
            let fileTypeNameMap = {};

            let obj = JSON.parse(json);
            if (obj.hasOwnProperty('filetypes') && Array.isArray(obj['filetypes'])) {
                obj['filetypes'].forEach(ft => {
                    let typename = ft['type'];
                    let extensions = ft['extensions'];
                    fileTypeExtMap[typename] = common.setFromArray(extensions);
                    if (ft.hasOwnProperty('names')) {
                        fileTypeNameMap[typename] = common.setFromArray(ft['names']);
                    } else {
                        fileTypeNameMap[typename] = [];
                    }
                });
            } else throw new Error("Invalid filetypes file: " + config.FILETYPESJSONPATH);

            fileTypeExtMap.text = [].concat(fileTypeExtMap.text, fileTypeExtMap.code, fileTypeExtMap.xml);
            fileTypeNameMap.text = [].concat(fileTypeNameMap.text, fileTypeNameMap.code, fileTypeNameMap.xml);

            return [fileTypeExtMap, fileTypeNameMap];
        })();
    }

    getFileType(filename) {
        if (this.isCodeFile(filename))
            return FileType.CODE;
        if (this.isXmlFile(filename))
            return FileType.XML;
        if (this.isTextFile(filename))
            return FileType.TEXT;
        if (this.isBinaryFile(filename))
            return FileType.BINARY;
        if (this.isArchiveFile(filename))
            return FileType.ARCHIVE;
        return FileType.UNKNOWN;
    }

    getFileTypeAsync(filename, cb) {
        try {
            if (this.isCodeFile(filename))
                return cb(null, FileType.CODE);
            if (this.isXmlFile(filename))
                return cb(null, FileType.XML);
            if (this.isTextFile(filename))
                return cb(null, FileType.TEXT);
            if (this.isBinaryFile(filename))
                return cb(null, FileType.BINARY);
            if (this.isArchiveFile(filename))
                return cb(null, FileType.ARCHIVE);
        } catch (err) {
            return cb(err);
        }
        return cb(null, FileType.UNKNOWN);
    }

    isArchiveFile(filename) {
        return this.fileTypeNameMap.archive.indexOf(filename) > -1
            || this.fileTypeExtMap.archive.indexOf(getExtension(filename)) > -1;
    }

    isBinaryFile(filename) {
        return this.fileTypeNameMap.binary.indexOf(filename) > -1
            || this.fileTypeExtMap.binary.indexOf(getExtension(filename)) > -1;
    }

    isCodeFile(filename) {
        return this.fileTypeNameMap.code.indexOf(filename) > -1
            || this.fileTypeExtMap.code.indexOf(getExtension(filename)) > -1;
    }

    isTextFile(filename) {
        return this.fileTypeNameMap.text.indexOf(filename) > -1
            || this.fileTypeExtMap.text.indexOf(getExtension(filename)) > -1;
    }

    isXmlFile(filename) {
        return this.fileTypeNameMap.xml.indexOf(filename) > -1
            || this.fileTypeExtMap.xml.indexOf(getExtension(filename)) > -1;
    }

    isUnknownFile(filename) {
        return this.getFileType(filename) === FileType.UNKNOWN;
    }
}

FileTypes.fromName = (name) => {
    if (name.toUpperCase() === 'TEXT')
        return FileType.TEXT;
    if (name.toUpperCase() === 'BINARY')
        return FileType.BINARY;
    if (name.toUpperCase() === 'ARCHIVE')
        return FileType.ARCHIVE;
    if (name.toUpperCase() === 'CODE')
        return FileType.CODE;
    if (name.toUpperCase() === 'XML')
        return FileType.XML;
    return FileType.UNKNOWN;
};

FileTypes.toName = (fileType) => {
    if (fileType === FileType.ARCHIVE)
        return 'ARCHIVE';
    if (fileType === FileType.BINARY)
        return 'BINARY';
    if (fileType === FileType.CODE)
        return 'CODE';
    if (fileType === FileType.TEXT)
        return 'TEXT';
    if (fileType === FileType.XML)
        return 'XML';
    return 'UNKNOWN';
};

exports.FileTypes = FileTypes;
