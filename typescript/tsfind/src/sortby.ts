/*
 * filetype.ts
 *
 * FileType enum
 *
 */

'use strict';

export enum SortBy {
    FilePath,
    FileName,
    FileType
}

export function nameToSortBy(name: string): SortBy {
    const uname: string = name.toUpperCase();
    if (uname === 'NAME')
        return SortBy.FileName;
    if (uname === 'TYPE')
        return SortBy.FileType;
    return SortBy.FilePath;
}

export function sortByToName(sortBy: SortBy): string {
    if (sortBy === SortBy.FileName)
        return 'NAME';
    if (sortBy === SortBy.FileType)
        return 'TYPE';
    return 'PATH';
}
