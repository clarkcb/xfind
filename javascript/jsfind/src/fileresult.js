/*
 * fileresult.js
 *
 * encapsulates a file to be found
 */

const path = require('path');

class FileResult {
    'use strict'

    constructor(pathname, filename, filetype, stat) {
        this.containerSeparator = '!';
        this.containers = [];
        this.pathname = pathname;
        this.filename = filename;
        this.filetype = filetype;
        this.stat = stat;
    }

    relativePath() {
        if (this.pathname === '.' || this.pathname === './') return './' + this.filename;
        return path.join(this.pathname, this.filename);
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
