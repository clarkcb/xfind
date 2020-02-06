/*
 * fileutil.js
 *
 * file-related utility functions
 */

"use strict";

const fs = require('fs');
const path = require('path');

const common = require('./common.js');

function FileUtil() {}

FileUtil.expandPath = function (filepath) {
    let idx = filepath.indexOf('~');
    return idx === 0 ? process.env.HOME + filepath.substring(1) : filepath;
};

FileUtil.getExtension = function (filepath) {
    let f = path.basename(filepath);
    let idx = f.lastIndexOf('.');
    if (idx > 0 && idx < f.length-1) {
        return f.substring(idx+1);
    } else {
        return '';
    }
};

FileUtil.getFileContents = function (filepath, encoding) {
    return fs.readFileSync(filepath, encoding).toString();
};

FileUtil.getFileContentsAsync = function (filepath, encoding, cb) {
    fs.readFile(filepath, encoding, (err, data) => {
        if (err) {
            common.log("An error occurred trying to read file: " + filepath);
            throw err;
        }
        cb(data.toString());
    });
};

FileUtil.getFileLines = function (filepath, encoding) {
    return FileUtil.getFileContents(filepath, encoding).split(/\r?\n/);
};

FileUtil.getFileLinesAsync = function (filepath, encoding, cb) {
    FileUtil.getFileContentsAsync(filepath, encoding, contents => cb(contents.split(/\r?\n/)));
};

FileUtil.getRelativePath = function (filepath, startpath) {
    if (startpath === '.' && filepath.startsWith(process.env.HOME)) {
        return '.' + filepath.substring(process.env.HOME.length);
    }
};

FileUtil.isDotDir = function (filepath) {
    return ['.', '..'].indexOf(filepath) > -1;
};

FileUtil.isHidden = function (filepath) {
    let f = path.basename(filepath);
    return f.length > 1 && f.charAt(0) === '.' && !FileUtil.isDotDir(f);
};

exports.FileUtil = FileUtil;
