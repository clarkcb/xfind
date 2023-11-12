/*jshint esnext: true */
/*
 * filetype.js
 *
 * FileType "static" class
 *
 */

const FileType = {
    UNKNOWN: 1,
    ARCHIVE: 2,
    AUDIO:   3,
    BINARY:  4,
    CODE:    5,
    FONT:    6,
    IMAGE:   7,
    TEXT:    8,
    VIDEO:   9,
    XML:     10
};
Object.freeze(FileType);

exports.FileType = FileType;
