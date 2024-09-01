/*
 * filetype.js
 *
 * FileType "static" class
 *
 */

const FileType = {
    UNKNOWN: 0,
    ARCHIVE: 1,
    AUDIO:   2,
    BINARY:  3,
    CODE:    4,
    FONT:    5,
    IMAGE:   6,
    TEXT:    7,
    VIDEO:   8,
    XML:     9
};
Object.freeze(FileType);

exports.FileType = FileType;
