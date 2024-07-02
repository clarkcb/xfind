/*
 * fileutil.js
 *
 * file-related utility functions
 */

'use strict';

import * as fs from 'fs';
import * as path from 'path';

import * as config from './config';

export class FileUtil {

    public static expandPath(filePath: string): string {
        if (filePath[0] === "~") {
            return config.HOME + filePath.substring(1);
        }
        return filePath;
    }

    public static getExtension(filePath: string): string {
        const f: string = path.basename(filePath);
        const idx: number = f.lastIndexOf('.');
        if (idx > 0 && idx < f.length - 1) {
            return f.substring(idx + 1);
        }
        return '';
    }

    public static getFileContentsSync(filePath: string, encoding: BufferEncoding='utf-8'): string {
        return fs.readFileSync(filePath, encoding).toString();
    }

    public static getFileContents(filePath: string, encoding: BufferEncoding='utf-8') {
        return new Promise((resolve, reject) => {
            fs.readFile(filePath, { encoding }, (err, data) => {
                if (err) {
                    reject(err);
                }
                resolve(data.toString());
            });
        });
    }

    public static getFileLinesSync(filePath: string, encoding: BufferEncoding='utf-8'): string[] {
        return FileUtil.getFileContentsSync(filePath, encoding).split(/\r?\n/);
    }

    public static getRelativePath(filePath: string, startpath: string): string {
        if (startpath === '.' && filePath.startsWith(config.HOME)) {
            return '.' + filePath.substring(config.HOME.length);
        }
        return filePath;
    }

    public static isDotDir(filePath: string): boolean {
        return ['.', './', '..', '../'].indexOf(filePath) > -1;
    }

    public static isHidden(filePath: string): boolean {
        const f: string = path.basename(filePath);
        return f.length > 1 && f.charAt(0) == '.' && !FileUtil.isDotDir(f);
    }
}
