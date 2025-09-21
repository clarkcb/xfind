/*
 * fileutil.js
 *
 * file-related utility functions
 */

'use strict';

import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';

import * as config from './config';
import {StringUtil} from "./stringutil";

export const ENOENT = 'ENOENT';
export const EACCES = 'EACCES';

const DOT_DIRS = ['.', '..', './', '../'];

export class FileUtil {

    public static expandPath(filePath: string): string {
        filePath = StringUtil.trimFromEnd(filePath, '/');
        if (filePath.indexOf("~") === 0) {
            const userPath = os.homedir();
            if (filePath === "~") {
                return userPath;
            }
            if (filePath.startsWith("~/")) {
                return path.join(userPath, filePath.substring(2));
            }
            let homePath = path.dirname(userPath);
            return path.join(homePath, filePath.substring(1));
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
        return DOT_DIRS.indexOf(filePath) > -1;
    }

    public static isHiddenName(fileName: string): boolean {
        return fileName.length > 1 && fileName.charAt(0) === '.' && !FileUtil.isDotDir(fileName);
    }

    public static isHiddenPath(filePath: string): boolean {
        const elems = filePath.split(path.sep).filter(p => p !== '');
        return (elems.some((p) => FileUtil.isHiddenName(p)));
    }
}
