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
        switch (name.toLowerCase()) {
            case 'filename':
            case 'name':
                return SortBy.FileName;
            case 'filesize':
            case 'size':
                return SortBy.FileSize;
            case 'filetype':
            case 'type':
                return SortBy.FileType;
            case 'lastmod':
                return SortBy.LastMod;
            default:
                return SortBy.FilePath;
        }
    }

    public static sortByToName(sortBy: SortBy): string {
        switch (sortBy) {
            case SortBy.FileName:
                return 'filename';
            case SortBy.FileSize:
                return 'filesize';
            case SortBy.FileType:
                return 'filetype';
            case SortBy.LastMod:
                return 'lastmod';
            default:
                return 'filepath';
        }
    }
}
