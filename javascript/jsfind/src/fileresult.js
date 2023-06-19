/*
 * fileresult.js
 *
 * encapsulates a file to be found
 */

const path = require('path');

class FileResult {
    'use strict'

    containerSeparator = '!';
    containers = [];
    path = '';
    fileName = '';
    stat = null;

    constructor(path, fileName, fileType, stat) {
        this.path = path;
        this.fileName = fileName;
        this.fileType = fileType;
        this.stat = stat;
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
