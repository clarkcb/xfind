/*jshint esnext: true */
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

exports.SortBy = SortBy;

exports.nameToSortBy = name => {
    if (name.toUpperCase() === 'NAME')
        return SortBy.FILENAME;
    if (name.toUpperCase() === 'SIZE')
        return SortBy.FILESIZE;
    if (name.toUpperCase() === 'TYPE')
        return SortBy.FILETYPE;
    if (name.toUpperCase() === 'LASTMOD')
        return SortBy.LASTMOD;
    return SortBy.FILEPATH;
};

exports.sortByToName = (sortBy) => {
    if (sortBy === SortBy.FILENAME)
        return 'NAME';
    if (sortBy === SortBy.FILESIZE)
        return 'SIZE';
    if (sortBy === SortBy.FILETYPE)
        return 'TYPE';
    if (sortBy === SortBy.LASTMOD)
        return 'LASTMOD';
    return 'PATH';
};
