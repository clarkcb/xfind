/*
 * fileresult.js
 *
 * encapsulates a file to be found
 */
const {FileType} = require('./filetype');

class FileResult {
    containerSeparator = '!';
    containers = [];
    filePath = '';
    fileType = FileType.UNKNOWN;
    fileSize = 0;
    lastMod = 0;

    constructor(filePath, fileType, fileSize, lastMod) {
        this.filePath = filePath;
        this.fileType = fileType;
        this.fileSize = fileSize;
        this.lastMod = lastMod;
    }

    toString() {
        let s = '';
        if (this.containers.length > 0) {
            s = this.containers.join(this.containerSeparator) + this.containerSeparator;
        }
        s += this.filePath;

        return s;
    }
}

exports.FileResult = FileResult;
