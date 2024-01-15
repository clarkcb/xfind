/*
 * sortby.js
 *
 * SortBy "static" class
 *
 */

const SortBy = {
    FILEPATH: 1,
    FILENAME: 2,
    FILESIZE: 3,
    FILETYPE: 4,
    LASTMOD: 5
};
Object.freeze(SortBy);

// exports.SortBy = SortBy;

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

const sortByToName = (sortBy) => {
    switch (sortBy) {
        case SortBy.FILENAME:
            return 'filename';
        case SortBy.FILESIZE:
            return 'filesize';
        case SortBy.FILETYPE:
            return 'filetype';
        case SortBy.LASTMOD:
            return 'lastmod';
        default:
            return 'filepath';
    }
};

module.exports = {SortBy, nameToSortBy, sortByToName};
