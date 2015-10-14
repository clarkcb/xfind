/*
 * fileutil.js
 *
 * file-related utility functions
 */

"use strict";

var fs = require('fs');
var path = require('path');

var common = require('./common.js');

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

FileUtil.getFileContents = function (filepath) {
    return fs.readFileSync(filepath).toString();
};

FileUtil.getFileContentsAsync = function (filepath, cb) {
    fs.readFile(filepath, (err, data) => {
        if (err) {
            common.log("An error occurred trying to read file: " + filepath);
            throw err;
        }
        cb(data.toString());
    });
};

FileUtil.getFileLines = function (filepath) {
    return FileUtil.getFileContents(filepath).split(/\r?\n/);
};

FileUtil.getFileLinesAsync = function (filepath, cb) {
    FileUtil.getFileContentsAsync(filepath, contents => cb(contents.split(/\r?\n/)));
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
