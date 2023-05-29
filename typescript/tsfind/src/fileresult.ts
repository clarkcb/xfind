/*
 * fileresult.js
 *
 * encapsulates a file to be found
 */

'use strict';

import {FileType} from './filetype';

import * as path from 'path';
import * as fs from "fs";

export class FileResult {
    containerSeparator = '!';
    containers: string[] = [];
    path: string;
    fileName: string;
    fileType: FileType;
    stat: fs.Stats | null;

    constructor(path: string, fileName: string, fileType: FileType, stat: fs.Stats | null) {
        this.path = path;
        this.fileName = fileName;
        this.fileType = fileType;
        this.stat = stat;
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
