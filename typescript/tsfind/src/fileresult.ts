/*
 * fileresult.js
 *
 * encapsulates a file to be found
 */

'use strict';

import {FileType} from './filetype';

export class FileResult {
    containerSeparator = '!';
    containers: string[] = [];
    filePath: string;
    fileType: FileType;
    fileSize: number = 0;
    lastMod: number = 0;

    constructor(filePath: string, fileType: FileType, fileSize: number, lastMod: number) {
        this.filePath = filePath;
        this.fileType = fileType;
        this.fileSize = fileSize;
        this.lastMod = lastMod;
    }

    public toString(): string {
        let s = '';
        if (this.containers.length > 0) {
            s = this.containers.join(this.containerSeparator) + this.containerSeparator;
        }
        s += this.filePath;
        return s;
    }
}
