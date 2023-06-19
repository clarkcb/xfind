/*
 * sortutil.ts
 *
 * SortUtil class
 *
 */

'use strict';

import {SortBy} from './sortby';

export class SortUtil {
    public static nameToSortBy(name: string): SortBy {
        const uname: string = name.toUpperCase();
        if (uname === 'NAME')
            return SortBy.FileName;
        if (uname === 'SIZE')
            return SortBy.FileSize;
        if (uname === 'TYPE')
            return SortBy.FileType;
        if (uname === 'LASTMOD')
            return SortBy.LastMod;
        return SortBy.FilePath;
    }

    public static sortByToName(sortBy: SortBy): string {
        if (sortBy === SortBy.FileName)
            return 'NAME';
        if (sortBy === SortBy.FileSize)
            return 'SIZE';
        if (sortBy === SortBy.FileType)
            return 'TYPE';
        if (sortBy === SortBy.LastMod)
            return 'LASTMOD';
        return 'PATH';
    }
}
