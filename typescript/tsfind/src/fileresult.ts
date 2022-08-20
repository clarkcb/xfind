/*
 * fileresult.js
 *
 * encapsulates a file to be found
 */

'use strict';

import {FileType} from './filetype';

import * as path from 'path';

export class FileResult {
    containerSeparator = '!';
    containers: string[] = [];
    pathname: string;
    filename: string;
    filetype: FileType;

    constructor(pathname: string, filename: string, filetype: FileType) {
        this.pathname = pathname;
        this.filename = filename;
        this.filetype = filetype;
    }

    public relativePath(): string {
        if (this.pathname === '.' || this.pathname === './') return './' + this.filename;
        return path.join(this.pathname, this.filename);
    }

    public toString(): string {
        let s = '';
        if (this.containers.length > 0) {
            s = this.containers.join(this.containerSeparator) + this.containerSeparator;
        }
        s += path.join(this.pathname, this.filename);
        return s;
    }
}
