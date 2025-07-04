/*
 * fileresultsorter.ts
 *
 * FileResultSorter class provides sorting of search results
 */

import {FileResult} from './fileresult';
import {FindSettings} from './findsettings';
import {SortBy} from "./sortby";

"use strict";

export class FileResultSorter {
    settings: FindSettings;

    constructor(settings: FindSettings) {
        this.settings = settings;
    }

    public cmpFileResultsByPath(fr1: FileResult, fr2: FileResult): number {
        const [path1, path2]: string[] = this.settings.sortCaseInsensitive ?
            [fr1.path.toLowerCase(), fr2.path.toLowerCase()] :
            [fr1.path, fr2.path];
        if (path1 === path2) {
            const [filename1, filename2]: string[] = this.settings.sortCaseInsensitive ?
                [fr1.fileName.toLowerCase(), fr2.fileName.toLowerCase()] :
                [fr1.fileName, fr2.fileName];
            if (filename1 === filename2) return 0;
            return filename1 < filename2 ? -1 : 1;
        }
        return path1 < path2 ? -1 : 1;
    }

    public cmpFileResultsByName(fr1: FileResult, fr2: FileResult): number {
        const [fileName1, fileName2]: string[] = this.settings.sortCaseInsensitive ?
            [fr1.fileName.toLowerCase(), fr2.fileName.toLowerCase()] :
            [fr1.fileName, fr2.fileName];
        if (fileName1 === fileName2) {
            const [path1, path2]: string[] = this.settings.sortCaseInsensitive ?
                [fr1.path.toLowerCase(), fr2.path.toLowerCase()] :
                [fr1.path, fr2.path];
            if (path1 === path2) return 0;
            return path1 < path2 ? -1 : 1;
        }
        return fileName1 < fileName2 ? -1 : 1;
    }

    public cmpFileResultsBySize(fr1: FileResult, fr2: FileResult): number {
        if (fr1.fileSize === fr2.fileSize) {
            return this.cmpFileResultsByPath(fr1, fr2);
        }
        return fr1.fileSize < fr2.fileSize ? -1 : 1;
    }

    public cmpFileResultsByType(fr1: FileResult, fr2: FileResult): number {
        if (fr1.fileType === fr2.fileType) {
            return this.cmpFileResultsByPath(fr1, fr2);
        }
        return fr1.fileType < fr2.fileType ? -1 : 1;
    }

    public cmpFileResultsByLastMod(fr1: FileResult, fr2: FileResult): number {
        if (fr1.lastMod === fr2.lastMod) {
            return this.cmpFileResultsByPath(fr1, fr2);
        }
        return fr1.lastMod < fr2.lastMod ? -1 : 1;
    }

    public getFileResultComparator(): (a: FileResult, b: FileResult) => number {
        if (this.settings.sortDescending) {
            switch (this.settings.sortBy) {
                case SortBy.FileName:
                    return (a, b) => this.cmpFileResultsByName(b, a);
                case SortBy.FileSize:
                    return (a, b) => this.cmpFileResultsBySize(b, a);
                case SortBy.FileType:
                    return (a, b) => this.cmpFileResultsByType(b, a);
                case SortBy.LastMod:
                    return (a, b) => this.cmpFileResultsByLastMod(b, a);
                default:
                    return (a, b) => this.cmpFileResultsByPath(b, a);
            }
        }
        switch (this.settings.sortBy) {
            case SortBy.FileName:
                return (a, b) => this.cmpFileResultsByName(a, b);
            case SortBy.FileSize:
                return (a, b) => this.cmpFileResultsBySize(a, b);
            case SortBy.FileType:
                return (a, b) => this.cmpFileResultsByType(a, b);
            case SortBy.LastMod:
                return (a, b) => this.cmpFileResultsByLastMod(a, b);
            default:
                return (a, b) => this.cmpFileResultsByPath(a, b);
        }
    }

    public sort(fileResults: FileResult[]): void {
        const sortComparator = this.getFileResultComparator();
        fileResults.sort(sortComparator);
    }
}
