/*
 * fileresult.js
 *
 * encapsulates a file to be found
 */
const {FileType} = require('./filetype');

const path = require('path');

class FileResult {
    containerSeparator = '!';
    containers = [];
    path = '';
    fileName = '';
    fileType = FileType.UNKNOWN;
    fileSize = 0;
    lastMod = 0;

    constructor(path, fileName, fileType, fileSize, lastMod) {
        this.path = path;
        this.fileName = fileName;
        this.fileType = fileType;
        this.fileSize = fileSize;
        this.lastMod = lastMod;
    }

    relativePath() {
        if (this.path === '.' || this.path === './') return './' + this.fileName;
        return path.join(this.path, this.fileName);
    }

    toString() {
        let s = '';
        if (this.containers.length > 0) {
            s = this.containers.join(this.containerSeparator) + this.containerSeparator;
        }
        s += this.relativePath();

        return s;
    }
}

exports.FileResult = FileResult;
