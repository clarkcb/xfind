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
    path: string;
    fileName: string;
    fileType: FileType;
    fileSize: number = 0;
    lastMod: number = 0;

    constructor(path: string, fileName: string, fileType: FileType, fileSize: number, lastMod: number) {
        this.path = path;
        this.fileName = fileName;
        this.fileType = fileType;
        this.fileSize = fileSize;
        this.lastMod = lastMod;
    }

    public relativePath(): string {
        if (this.path === '.' || this.path === './') return './' + this.fileName;
        return path.join(this.path, this.fileName);
    }

    public toString(): string {
        let s = '';
        if (this.containers.length > 0) {
            s = this.containers.join(this.containerSeparator) + this.containerSeparator;
        }
        s += path.join(this.path, this.fileName);
        return s;
    }
}
