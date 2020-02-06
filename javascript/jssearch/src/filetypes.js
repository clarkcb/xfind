/*
 * filetypes.js
 *
 * identifies file types (archive, binary, text, unknown)
 */

var common = require('./common.js');
var config = require('./config.js');
var FileType = require('./filetype.js').FileType;
var FileUtil = require('./fileutil.js').FileUtil;

function FileTypes() {
    "use strict";
    let self = this;

    const fileTypeMap = (() => {
        let fs = require('fs');

        let json = '';
        if (fs.existsSync(FileUtil.expandPath(config.FILETYPESJSONPATH))) {
            json = fs.readFileSync(FileUtil.expandPath(config.FILETYPESJSONPATH)).toString();
        } else {
            throw new Error('File not found: ' + config.FILETYPESJSONPATH);
        }

        let fileTypeMap = {};

        let obj = JSON.parse(json);
        if (obj.hasOwnProperty('filetypes') && Array.isArray(obj['filetypes'])) {
            obj['filetypes'].forEach(ft => {
                let typename = ft['type'];
                let extensions = ft['extensions'];
                fileTypeMap[typename] = common.setFromArray(extensions);
            });
        } else throw new Error("Invalid filetypes file: " + config.FILETYPESJSONPATH);

        fileTypeMap.text = [].concat(fileTypeMap.text, fileTypeMap.code, fileTypeMap.xml);
        fileTypeMap.searchable = [].concat(fileTypeMap.text, fileTypeMap.binary,
            fileTypeMap.archive);

        return fileTypeMap;
    })();

    this.getFileType = function (filename) {
        if (self.isCodeFile(filename))
            return FileType.CODE;
        if (self.isXmlFile(filename))
            return FileType.XML;
        if (self.isTextFile(filename))
            return FileType.TEXT;
        if (self.isBinaryFile(filename))
            return FileType.BINARY;
        if (self.isArchiveFile(filename))
            return FileType.ARCHIVE;
        return FileType.UNKNOWN;
    };

    this.getFileTypeAsync = function (filename, cb) {
        if (self.isCodeFile(filename))
            return cb(FileType.CODE);
        if (self.isXmlFile(filename))
            return cb(FileType.XML);
        if (self.isTextFile(filename))
            return cb(FileType.TEXT);
        if (self.isBinaryFile(filename))
            return cb(FileType.BINARY);
        if (self.isArchiveFile(filename))
            return cb(FileType.ARCHIVE);
        cb(FileType.UNKNOWN);
    };

    this.isArchiveFile = function (filename) {
        let ext = FileUtil.getExtension(filename);
        return fileTypeMap.archive.indexOf(ext) > -1;
    };

    this.isBinaryFile = function (filename) {
        let ext = FileUtil.getExtension(filename);
        return fileTypeMap.binary.indexOf(ext) > -1;
    };

    this.isCodeFile = function (filename) {
        let ext = FileUtil.getExtension(filename);
        return fileTypeMap.code.indexOf(ext) > -1;
    };

    this.isSearchableFile = function (filename) {
        let ext = FileUtil.getExtension(filename);
        return fileTypeMap.searchable.indexOf(ext) > -1;
    };

    this.isTextFile = function (filename) {
        let ext = FileUtil.getExtension(filename);
        return fileTypeMap.text.indexOf(ext) > -1;
    };

    this.isXmlFile = function (filename) {
        let ext = FileUtil.getExtension(filename);
        return fileTypeMap.xml.indexOf(ext) > -1;
    };

    this.isUnknownFile = function (filename) {
        let ext = FileUtil.getExtension(filename);
        return fileTypeMap.searchable.indexOf(ext) === -1;
    };
}

FileTypes.fromName = function (name) {
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

FileTypes.toName = function (fileType) {
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
