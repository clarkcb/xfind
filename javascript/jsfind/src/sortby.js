/*
 * sortby.js
 *
 * SortBy "static" class
 *
 */

const SortBy = Object.freeze({
    FILEPATH: 'filepath',
    FILENAME: 'filename',
    FILESIZE: 'filesize',
    FILETYPE: 'filetype',
    LASTMOD: 'lastmod'
});

const nameToSortBy = name => {
    switch (name.toLowerCase()) {
        case 'filename':
        case 'name':
            return SortBy.FILENAME;
        case 'filesize':
        case 'size':
            return SortBy.FILESIZE;
        case 'filetype':
        case 'type':
            return SortBy.FILETYPE;
        case 'lastmod':
            return SortBy.LASTMOD;
        default:
            return SortBy.FILEPATH;
    }
};

module.exports = {SortBy, nameToSortBy};
