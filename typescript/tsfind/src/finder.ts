/*
 * finder.ts
 *
 * performs the finding based on the given FindSettings instance
 */

'use strict';

import * as assert from 'assert';
import * as fs from 'fs';
import {stat} from 'fs/promises';
import * as path from 'path';

import * as common from './common';
import {FileResult} from './fileresult';
import {FileType} from './filetype';
import {FileTypes} from './filetypes';
import {FileUtil} from './fileutil';
import {FindError} from './finderror';
import {FindSettings} from './findsettings';
import {SortBy} from "./sortby";

export class Finder {
    _settings: FindSettings;

    constructor(settings: FindSettings) {
        this._settings = settings;
        this.validateSettings();
    }

    private validateSettings(): void {
        try {
            assert.ok(this._settings.paths.length > 0, 'Startpath not defined');
            for (const p of this._settings.paths) {
                // await access(p, fs.constants.F_OK | fs.constants.R_OK);
                fs.accessSync(p, fs.constants.F_OK | fs.constants.R_OK);
                // const stat = await lstat(p);
                const stat = fs.lstatSync(p);
                if (!stat.isDirectory() && !stat.isFile()) {
                    assert.ok(false, 'Startpath is unsupported file type');
                }
            }
            if (this._settings.maxDepth > -1 && this._settings.minDepth > -1) {
                assert.ok(this._settings.maxDepth >= this._settings.minDepth,
                    'Invalid range for mindepth and maxdepth');
            }
            if (this._settings.maxLastMod > 0 && this._settings.minLastMod > 0) {
                assert.ok(this._settings.maxLastMod >= this._settings.minLastMod,
                    'Invalid range for minlastmod and maxlastmod');
            }
            if (this._settings.maxSize > 0 && this._settings.minSize > 0) {
                assert.ok(this._settings.maxSize >= this._settings.minSize,
                    'Invalid range for minsize and maxsize');
            }

        } catch (err: Error | any) {
            let msg = err.message;
            if (err.code === 'ENOENT') {
                msg = 'Startpath not found';
            } else if (err.code === 'EACCES') {
                msg = 'Startpath not readable';
            }
            throw new FindError(msg);
        }
    }

    private static matchesAnyString(s: string, elements: string[]): boolean {
        return elements.indexOf(s) > -1;
    }

    private static matchesAnyPattern(s: string, patterns: RegExp[]): boolean {
        return patterns.some((p: RegExp) => s.search(p) > -1);
    }

    private static matchesAnyFileType(ft: FileType, fileTypes: FileType[]): boolean {
        return fileTypes.indexOf(ft) > -1;
    }

    public isMatchingDir(dir: string): boolean {
        if (FileUtil.isDotDir(dir)) {
            return true;
        }
        if (!this._settings.includeHidden) {
            const nonDotElems = dir.split(path.sep).filter((p: string) => !Finder.matchesAnyString(p, ['.','..']));
            if (nonDotElems.length === 0) {
                return true;
            }
            if (nonDotElems.some((p: string) => FileUtil.isHidden(p))) {
                return false;
            }
        }
        if (this._settings.inDirPatterns.length && !Finder.matchesAnyPattern(dir,
                this._settings.inDirPatterns)) {
            return false;
        }
        return !(this._settings.outDirPatterns.length && Finder.matchesAnyPattern(dir,
            this._settings.outDirPatterns));
    }

    public isMatchingExtension(ext: string, inExtensions: string[], outExtensions: string[]): boolean {
        return ((inExtensions.length === 0 || Finder.matchesAnyString(ext, inExtensions))
            && (outExtensions.length === 0 || !Finder.matchesAnyString(ext, outExtensions)));
    }

    public hasMatchingArchiveExtension(fr: FileResult): boolean {
        if (this._settings.inArchiveExtensions.length || this._settings.outArchiveExtensions.length) {
            const ext: string = FileUtil.getExtension(fr.fileName);
            return this.isMatchingExtension(ext, this._settings.inArchiveExtensions, this._settings.outArchiveExtensions);
        }
        return true;
    }

    public hasMatchingExtension(fr: FileResult): boolean {
        if (this._settings.inExtensions.length || this._settings.outExtensions.length) {
            const ext: string = FileUtil.getExtension(fr.fileName);
            return this.isMatchingExtension(ext, this._settings.inExtensions, this._settings.outExtensions);
        }
        return true;
    }

    public isMatchingArchiveFileName(fileName: string): boolean {
        return ((this._settings.inArchiveFilePatterns.length === 0 ||
                Finder.matchesAnyPattern(fileName, this._settings.inArchiveFilePatterns))
            && (this._settings.outArchiveFilePatterns.length === 0 ||
                !Finder.matchesAnyPattern(fileName, this._settings.outArchiveFilePatterns)));
    }

    public isMatchingFileName(fileName: string): boolean {
        return ((this._settings.inFilePatterns.length === 0 ||
                Finder.matchesAnyPattern(fileName, this._settings.inFilePatterns))
            && (this._settings.outFilePatterns.length === 0 ||
                !Finder.matchesAnyPattern(fileName, this._settings.outFilePatterns)));
    }

    public isMatchingFileType(fileType: FileType): boolean {
        return ((this._settings.inFileTypes.length === 0 ||
                Finder.matchesAnyFileType(fileType, this._settings.inFileTypes))
            && (this._settings.outFileTypes.length === 0 ||
                !Finder.matchesAnyFileType(fileType, this._settings.outFileTypes)));
    }

    public isMatchingFileSize(fileSize: number): boolean {
        return ((this._settings.maxSize === 0 || fileSize <= this._settings.maxSize) &&
            (this._settings.minSize === 0 || fileSize >= this._settings.minSize));
    }

    public isMatchingLastMod(lastMod: number): boolean {
        return ((this._settings.maxLastMod === 0 || lastMod <= this._settings.maxLastMod) &&
            (this._settings.minLastMod === 0 || lastMod >= this._settings.minLastMod));
    }

    public isMatchingFileResult(fr: FileResult): boolean {
        return this.hasMatchingExtension(fr)
            && this.isMatchingFileName(fr.fileName)
            && this.isMatchingFileType(fr.fileType)
            && this.isMatchingFileSize(fr.fileSize)
            && this.isMatchingLastMod(fr.lastMod);
    }

    public isMatchingArchiveFileResult(fr: FileResult): boolean {
        return this.hasMatchingArchiveExtension(fr)
            && this.isMatchingArchiveFileName(fr.fileName);
    }

    public filePathToFileResult(fp: string, stat: fs.Stats | null = null): FileResult {
        const dirname = path.dirname(fp) || '.';
        const fileName = path.basename(fp);
        const fileType = FileTypes.getFileType(fileName);
        let fileSize = 0;
        let lastMod = 0;
        if (this._settings.needLastMod() || this._settings.needSize()) {
            stat = stat || fs.statSync(fp);
            if (this._settings.needSize()) fileSize = stat.size;
            if (this._settings.needLastMod()) lastMod = stat.mtime.getTime();
        }
        return new FileResult(dirname, fileName, fileType, fileSize, lastMod);
    }

    public filterToFileResult(fp: string, stat: fs.Stats | null = null): FileResult | null {
        if (!this._settings.includeHidden && FileUtil.isHidden(fp)) {
            return null;
        }
        const fr = this.filePathToFileResult(fp, stat);
        if (fr.fileType === FileType.Archive) {
            if (this._settings.includeArchives && this.isMatchingArchiveFileResult(fr)) {
                return fr;
            }
            return null;
        }
        if (!this._settings.archivesOnly && this.isMatchingFileResult(fr)) {
            return fr;
        }
        return null;
    }

    private recGetFileResults(currentDir: string, depth: number): FileResult[] {
        if (this._settings.maxDepth > 0 && depth > this._settings.maxDepth) {
            return [];
        }
        const findDirs: string[] = [];
        let fileResults: FileResult[] = [];
        fs.readdirSync(currentDir).map((f: string) => {
            return path.join(currentDir, f);
        }).forEach((fp: string) => {
            const stats = fs.statSync(fp);
            if (stats.isDirectory()) {
                if (this._settings.recursive && this.isMatchingDir(fp)) {
                    findDirs.push(fp);
                }
            } else if (stats.isFile()) {
                if (depth >= this._settings.minDepth) {
                    const fr = this.filterToFileResult(fp);
                    if (fr !== null) {
                        fileResults.push(fr);
                    }
                }
            }
        });
        findDirs.forEach(d => {
            fileResults = fileResults.concat(this.recGetFileResults(d, depth + 1));
        });
        return fileResults;
    }

    private async getFileResults(startPath: string): Promise<FileResult[]> {
        let fileResults: FileResult[] = [];
        const stats = await stat(startPath);
        if (stats.isDirectory()) {
            // if max_depth is zero, we can skip since a directory cannot be a result
            if (this._settings.maxDepth === 0) {
                return [];
            }
            if (this.isMatchingDir(startPath)) {
                fileResults = fileResults.concat(this.recGetFileResults(startPath, 1));
            } else {
                throw new FindError("Startpath does not match find settings");
            }
        } else if (stats.isFile()) {
            // if min_depth > zero, we can skip since the file is at depth zero
            if (this._settings.minDepth > 0) {
                return [];
            }
            const dirname = path.dirname(startPath) || '.';
            if (this.isMatchingDir(dirname)) {
                const fr = this.filterToFileResult(startPath);
                if (fr !== null) {
                    fileResults.push(fr);
                } else {
                    throw new FindError("Startpath does not match find settings");
                }
            } else {
                throw new FindError("Startpath does not match find settings");
            }
        }
        return fileResults;
    }

    private cmpFileResultsByPath(fr1: FileResult, fr2: FileResult): number {
        const [path1, path2]: string[] = this._settings.sortCaseInsensitive ?
            [fr1.path.toLowerCase(), fr2.path.toLowerCase()] :
            [fr1.path, fr2.path];
        if (path1 === path2) {
            const [filename1, filename2]: string[] = this._settings.sortCaseInsensitive ?
                [fr1.fileName.toLowerCase(), fr2.fileName.toLowerCase()] :
                [fr1.fileName, fr2.fileName];
            return filename1 < filename2 ? -1 : 1;
        }
        return path1 < path2 ? -1 : 1;
    }

    private cmpFileResultsByName(fr1: FileResult, fr2: FileResult): number {
        const [fileName1, fileName2]: string[] = this._settings.sortCaseInsensitive ?
            [fr1.fileName.toLowerCase(), fr2.fileName.toLowerCase()] :
            [fr1.fileName, fr2.fileName];
        if (fileName1 === fileName2) {
            const [path1, path2]: string[] = this._settings.sortCaseInsensitive ?
                [fr1.path.toLowerCase(), fr2.path.toLowerCase()] :
                [fr1.path, fr2.path];
            return path1 < path2 ? -1 : 1;
        }
        return fileName1 < fileName2 ? -1 : 1;
    }

    private cmpFileResultsBySize(fr1: FileResult, fr2: FileResult): number {
        if (fr1.fileSize === fr2.fileSize) {
            return this.cmpFileResultsByPath(fr1, fr2);
        }
        return fr1.fileSize - fr2.fileSize;
    }

    private cmpFileResultsByType(fr1: FileResult, fr2: FileResult): number {
        if (fr1.fileType === fr2.fileType) {
            return this.cmpFileResultsByPath(fr1, fr2);
        }
        return fr1.fileType - fr2.fileType;
    }

    private cmpFileResultsByLastMod(fr1: FileResult, fr2: FileResult): number {
        if (fr1.lastMod === fr2.lastMod) {
            return this.cmpFileResultsByPath(fr1, fr2);
        }
        return fr1.lastMod - fr2.lastMod;
    }

    private sortFileResults(fileResults: FileResult[]): void {
        if (this._settings.sortBy === SortBy.FileName) {
            fileResults.sort((a, b) => this.cmpFileResultsByName(a, b));
        } else if (this._settings.sortBy === SortBy.FileSize) {
            fileResults.sort((a, b) => this.cmpFileResultsBySize(a, b));
        } else if (this._settings.sortBy === SortBy.FileType) {
            fileResults.sort((a, b) => this.cmpFileResultsByType(a, b));
        } else if (this._settings.sortBy === SortBy.LastMod) {
            fileResults.sort((a, b) => this.cmpFileResultsByLastMod(a, b));
        } else {
            fileResults.sort((a, b) => this.cmpFileResultsByPath(a, b));
        }
        if (this._settings.sortDescending) {
            fileResults.reverse();
        }
    }

    public async find(): Promise<FileResult[]> {
        // get the file results
        let fileResults: FileResult[] = [];

        const pathFileResultsArrays = await Promise.all(this._settings.paths.map(d => this.getFileResults(d)));
        pathFileResultsArrays.forEach(pathFileResults => {
            fileResults = fileResults.concat(pathFileResults);
        });

        this.sortFileResults(fileResults);
        return fileResults;
    }

    public getMatchingDirs(fileResults: FileResult[]): string[] {
        const dirs: string[] = fileResults.map(f => f.path);
        return common.setFromArray(dirs);
    }

    public printMatchingDirs(fileResults: FileResult[]): void {
        const dirs: string[] = this.getMatchingDirs(fileResults);
        if (dirs.length > 0) {
            common.log("\nMatching directories " + `(${dirs.length}):`);
            dirs.forEach(d => common.log(d));
        } else {
            common.log("\nMatching directories: 0");
        }
    }

    public getMatchingFiles(fileResults: FileResult[]): string[] {
        return fileResults.map(f => f.relativePath());
    }

    public printMatchingFiles(fileResults: FileResult[]): void {
        const files: string[] = this.getMatchingFiles(fileResults);
        if (files.length > 0) {
            common.log("\nMatching files " + `(${files.length}):`);
            files.forEach(f => common.log(f));
        } else {
            common.log("\nMatching files: 0");
        }
    }
}
