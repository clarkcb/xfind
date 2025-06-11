/*
 * fileutil.js
 *
 * file-related utility functions
 */

const fs = require('fs');
const os = require('os');
const path = require('path');
const common = require('./common');
const { trimFromEnd } = require('./stringutil')

const ENOENT = 'ENOENT';
const EACCES = 'EACCES';

class FileUtil {
    static expandPath(filePath) {
        filePath = trimFromEnd(filePath, '/\\');
        if (filePath.indexOf('~') === 0) {
            const userPath = os.homedir();
            if (filePath === '~') {
                return userPath;
            }
            if (filePath.startsWith('~/')) {
                return path.join(userPath, filePath.substring(2));
            }
            let homePath = path.dirname(userPath);
            return path.join(homePath, filePath.substring(1));
        }
        return filePath;
    }

    static getExtension(filePath) {
        let f = path.basename(filePath);
        let idx = f.lastIndexOf('.');
        if (idx > 0 && idx < f.length - 1) {
            return f.substring(idx+1);
        }
        return '';
    }

    static getFileContentsSync(filePath, encoding) {
        return fs.readFileSync(filePath, { encoding }).toString();
    }

    static getFileContents(filePath, encoding) {
        return new Promise((resolve, reject) => {
            fs.readFile(filePath, { encoding }, (err, data) => {
                if (err) {
                    common.log('An error occurred trying to read file: ' + filePath);
                    reject(err);
                }
                resolve(data.toString());
            });
        });
    }

    static getFileLinesSync(filePath, encoding) {
        return FileUtil.getFileContentsSync(filePath, encoding).split(/\r?\n/);
    }

    static getRelativePath(filePath, startpath) {
        if (startpath === '.' && filePath.startsWith(process.env.HOME)) {
            return '.' + filePath.substring(process.env.HOME.length);
        }
    }

    static isDotDir(filePath) {
        return ['.', '..', './', '../'].indexOf(filePath) > -1;
    }

    static isHidden(filePath) {
        let f = path.basename(filePath);
        return f.length > 1 && f.charAt(0) === '.' && !FileUtil.isDotDir(f);
    }
}

exports.FileUtil = FileUtil;
exports.EACCES = EACCES;
exports.ENOENT = ENOENT;
