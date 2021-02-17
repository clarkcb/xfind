/*
 * findfile.js
 *
 * encapsulates a file to be found
 */

const path = require('path');

class FindFile {
    'use strict'

    constructor(pathname, filename, filetype) {
        this.containerSeparator = '!';
        this.containers = [];
        this.pathname = pathname;
        this.filename = filename;
        this.filetype = filetype;
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

exports.FindFile = FindFile;
