/*
 * fileutil.js
 *
 * file-related utility functions
 */

const fs = require('fs');
const path = require('path');
const common = require('./common');
const { promisify } = require('util');
const readFileAsync = promisify(fs.readFile);

class FileUtil {
    static expandPath(filePath) {
        let idx = filePath.indexOf('~');
        return idx === 0 ? process.env.HOME + filePath.substring(1) : filePath;
    }

    static expandPathAsync(filePath, cb) {
        let idx = filePath.indexOf('~');
        return cb(null, idx === 0 ? process.env.HOME + filePath.substring(1) : filePath);
    }

    static getExtension(filePath) {
        let f = path.basename(filePath);
        let idx = f.lastIndexOf('.');
        if (idx > 0 && idx < f.length-1) {
            return f.substring(idx+1);
        } else {
            return '';
        }
    }

    static getExtensionAsync(filePath, cb) {
        try {
            let f = path.basename(filePath);
            let idx = f.lastIndexOf('.');
            if (idx > 0 && idx < f.length-1) {
                return cb(null, f.substring(idx+1));
            } else {
                return cb(null, '');
            }
        } catch (err) {
            return cb(err);
        }
    }

    static getFileContents(filePath, encoding) {
        return fs.readFileSync(filePath, encoding).toString();
    }

    static async getFileContentsAsync(filePath, encoding) {
        return await readFileAsync(filePath, encoding);
    }

    static getFileContentsCallback(filePath, encoding, cb) {
        fs.readFile(filePath, encoding, (err, data) => {
            if (err) {
                common.log('An error occurred trying to read file: ' + filePath);
                cb(err);
            }
            cb(null, data.toString());
        });
    }

    static getFileLines(filePath, encoding) {
        return FileUtil.getFileContents(filePath, encoding).split(/\r?\n/);
    }

    static getFileLinesAsync(filePath, encoding, cb) {
        FileUtil.getFileContentsCallback(filePath, encoding, (err, contents) => {
            if (err) {
                cb(err);
            }
            cb(null, contents.split(/\r?\n/));
        });
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
