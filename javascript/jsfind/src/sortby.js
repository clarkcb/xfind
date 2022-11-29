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
    FILETYPE: 3
};
Object.freeze(SortBy);

exports.SortBy = SortBy;

exports.nameToSortBy = name => {
    if (name.toUpperCase() === 'NAME')
        return SortBy.FILENAME;
    if (name.toUpperCase() === 'TYPE')
        return SortBy.FILETYPE;
    return SortBy.FILEPATH;
};

exports.sortByToName = (sortBy) => {
    if (sortBy === SortBy.FILENAME)
        return 'NAME';
    if (sortBy === SortBy.FILETYPE)
        return 'TYPE';
    return 'PATH';
};
