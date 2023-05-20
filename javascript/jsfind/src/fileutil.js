/*
 * fileutil.js
 *
 * file-related utility functions
 */

'use strict';

const fs = require('fs');
const path = require('path');

const common = require('./common');

const { promisify } = require('util');
const readFileAsync = promisify(fs.readFile);

exports.expandPath = filePath => {
    let idx = filePath.indexOf('~');
    return idx === 0 ? process.env.HOME + filePath.substring(1) : filePath;
};

exports.expandPathAsync = (filePath, cb) => {
    let idx = filePath.indexOf('~');
    return cb(null, idx === 0 ? process.env.HOME + filePath.substring(1) : filePath);
};

exports.getExtension = filePath => {
    try {
        let f = path.basename(filePath);
        let idx = f.lastIndexOf('.');
        if (idx > 0 && idx < f.length-1) {
            return f.substring(idx+1);
        } else {
            return '';
        }
    } catch (err) {
        throw err;
    }
};

exports.getExtensionAsync = (filePath, cb) => {
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
};

exports.getFileContents = (filePath, encoding) => {
    return fs.readFileSync(filePath, encoding).toString();
};

exports.getFileContentsAsync = async (filePath, encoding) => {
    try {
        let data = await readFileAsync(filePath, encoding);
        return data;
    } catch (err) {
        throw err;
    }
};

exports.getFileContentsCallback = (filePath, encoding, cb) => {
    fs.readFile(filePath, encoding, (err, data) => {
        if (err) {
            common.log('An error occurred trying to read file: ' + filePath);
            cb(err);
        }
        cb(null, data.toString());
    });
};

exports.getFileLines = (filePath, encoding) => {
    return exports.getFileContents(filePath, encoding).split(/\r?\n/);
};

exports.getFileLinesAsync = (filePath, encoding, cb) => {
    exports.getFileContentsCallback(filePath, encoding, (err, contents) => {
        if (err) {
            cb(err);
        }
        cb(null, contents.split(/\r?\n/));
    });
};

exports.getRelativePath = (filePath, startpath) => {
    if (startpath === '.' && filePath.startsWith(process.env.HOME)) {
        return '.' + filePath.substring(process.env.HOME.length);
    }
};

exports.isDotDir = filePath => {
    return ['.', '..', './', '../'].indexOf(filePath) > -1;
};

exports.isHidden = filePath => {
    let f = path.basename(filePath);
    return f.length > 1 && f.charAt(0) === '.' && !exports.isDotDir(f);
};
