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

import {FileResult} from './fileresult';
import {FileType} from './filetype';
import {FileTypes} from './filetypes';
import {FileUtil, ENOENT, EACCES} from './fileutil';
import {FindError} from './finderror';
import {FindSettings} from './findsettings';
import {SortBy} from './sortby';
import * as common from "./common";
import {FileResultFormatter} from "./fileresultformatter";
import {FileResultSorter} from "./fileresultsorter";

const startPathNotDefined = 'Startpath not defined';
const invalidRangeForMinDepthAndMaxDepth = 'Invalid range for mindepth and maxdepth';
const invalidRangeForMinLastModAndMaxLastMod = 'Invalid range for minlastmod and maxlastmod';
const invalidRangeForMinSizeAndMaxSize = 'Invalid range for minsize and maxsize';
const startPathNotFound = 'Startpath not found';
const startPathNotReadable = 'Startpath not readable';
const startPathDoesNotMatchFindSettings = 'Startpath does not match find settings';


export class Finder {
    _settings: FindSettings;

    constructor(settings: FindSettings) {
        this._settings = settings;
        this.validateSettings();
    }

    private validateSettings(): void {
        try {
            assert.ok(this._settings.paths.length > 0, startPathNotDefined);
            for (let p of this._settings.paths) {
                // Validate existence, accessibility and "findability" of file path (directory or regular file)
                try {
                    fs.accessSync(p, fs.constants.F_OK | fs.constants.R_OK);
                } catch (err: Error | any) {
                    p = FileUtil.expandPath(p);
                    fs.accessSync(p, fs.constants.F_OK | fs.constants.R_OK);
                }
            }
            if (this._settings.maxDepth > -1 && this._settings.minDepth > -1) {
                assert.ok(this._settings.maxDepth >= this._settings.minDepth,
                    invalidRangeForMinDepthAndMaxDepth);
            }
            if (this._settings.maxLastMod > 0 && this._settings.minLastMod > 0) {
                assert.ok(this._settings.maxLastMod >= this._settings.minLastMod,
                    invalidRangeForMinLastModAndMaxLastMod);
            }
            if (this._settings.maxSize > 0 && this._settings.minSize > 0) {
                assert.ok(this._settings.maxSize >= this._settings.minSize,
                    invalidRangeForMinSizeAndMaxSize);
            }

        } catch (err: Error | any) {
            let msg = err.message;
            if (err.code === ENOENT) {
                msg = startPathNotFound;
            } else if (err.code === EACCES) {
                msg = startPathNotReadable;
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

    public isMatchingArchiveFileResult(fr: FileResult): boolean {
        return this.hasMatchingArchiveExtension(fr)
            && this.isMatchingArchiveFileName(fr.fileName);
    }

    public isMatchingFileResult(fr: FileResult): boolean {
        return this.hasMatchingExtension(fr)
            && this.isMatchingFileName(fr.fileName)
            && this.isMatchingFileType(fr.fileType)
            && this.isMatchingFileSize(fr.fileSize)
            && this.isMatchingLastMod(fr.lastMod);
    }

    public filterToFileResult(fp: string, stat: fs.Stats | null = null): FileResult | null {
        if (!this._settings.includeHidden && FileUtil.isHidden(fp)) {
            return null;
        }
        const dirname = path.dirname(fp) || '.';
        const fileName = path.basename(fp);
        const fileType = FileTypes.getFileType(fileName);
        if (fileType === FileType.Archive
            && !this._settings.includeArchives
            && !this._settings.archivesOnly) {
            return null;
        }
        let fileSize = 0;
        let lastMod = 0;
        if (this._settings.needLastMod() || this._settings.needSize()) {
            stat = stat || fs.statSync(fp);
            if (this._settings.needSize()) fileSize = stat.size;
            if (this._settings.needLastMod()) lastMod = stat.mtime.getTime();
        }
        const fr = new FileResult(dirname, fileName, fileType, fileSize, lastMod);
        if (fr.fileType === FileType.Archive) {
            if (this.isMatchingArchiveFileResult(fr)) {
                return fr;
            }
            return null;
        }
        if (!this._settings.archivesOnly && this.isMatchingFileResult(fr)) {
            return fr;
        }
        return null;
    }

    private async recGetFileResults(currentDir: string, minDepth: number, maxDepth: number, currentDepth: number): Promise<FileResult[]> {
        let fileResults: FileResult[] = [];
        let recurse: boolean = true;
        if (currentDepth === maxDepth) {
            recurse = false;
        } else if (maxDepth > -1 && currentDepth > maxDepth) {
            return [];
        }
        const findDirs: string[] = [];
        let filePaths = fs.readdirSync(currentDir, { recursive: false })
            .map(f =>  path.join(currentDir, f.toString()));
        for (let filePath of filePaths) {
            let stats = fs.lstatSync(filePath);
            if (!stats.isSymbolicLink() || this._settings.followSymlinks) {
                stats = fs.statSync(filePath);
                if (stats.isDirectory() && recurse && this.isMatchingDir(filePath)) {
                    findDirs.push(filePath);
                } else if (stats.isFile() && (minDepth < 0 || currentDepth >= minDepth)) {
                    const fr = this.filterToFileResult(filePath);
                    if (fr !== null) {
                        fileResults.push(fr);
                    }
                }
            }
        }
        const subDirFileResultArrays = await Promise.all(findDirs.map(d => this.recGetFileResults(d, minDepth, maxDepth, currentDepth + 1)));
        subDirFileResultArrays.forEach(subDirFileResults => {
            fileResults = fileResults.concat(subDirFileResults);
        });
        return fileResults;
    }

    private async getFileResults(filePath: string): Promise<FileResult[]> {
        try {
            fs.accessSync(filePath, fs.constants.F_OK | fs.constants.R_OK);
        } catch (err) {
            filePath = FileUtil.expandPath(filePath);
        }
        const stats = await stat(filePath);
        if (stats.isDirectory()) {
            // if max_depth is zero, we can skip since a directory cannot be a result
            if (this._settings.maxDepth === 0) {
                return [];
            }
            if (this.isMatchingDir(filePath)) {
                let maxDepth = this._settings.maxDepth;
                if (!this._settings.recursive) {
                    maxDepth = 1;
                }
                return await this.recGetFileResults(filePath, this._settings.minDepth, maxDepth, 1);
            } else {
                throw new FindError(startPathDoesNotMatchFindSettings);
            }
        } else {
            // if min_depth > zero, we can skip since the file is at depth zero
            if (this._settings.minDepth > 0) {
                return [];
            }
            const dirname = path.dirname(filePath) || '.';
            if (this.isMatchingDir(dirname)) {
                const fr = this.filterToFileResult(filePath);
                if (fr !== null) {
                    return [fr];
                } else {
                    throw new FindError(startPathDoesNotMatchFindSettings);
                }
            } else {
                throw new FindError(startPathDoesNotMatchFindSettings);
            }
        }
    }

    public async find(): Promise<FileResult[]> {
        // get the file results
        let fileResults: FileResult[] = [];

        const pathFileResultsArrays = await Promise.all(this._settings.paths.map(d => this.getFileResults(d)));
        pathFileResultsArrays.forEach(pathFileResults => {
            fileResults = fileResults.concat(pathFileResults);
        });

        let fileResultSorter = new FileResultSorter(this._settings);
        fileResultSorter.sort(fileResults);
        return fileResults;
    }

    getMatchingDirs(fileResults: FileResult[]): string[] {
        const dirs: string[] = fileResults.map(f => f.path);
        return common.setFromArray(dirs);
    }

    printMatchingDirs(fileResults: FileResult[], formatter: FileResultFormatter): void {
        const dirs: string[] = this.getMatchingDirs(fileResults);
        if (dirs.length > 0) {
            common.log("\nMatching directories " + `(${dirs.length}):`);
            dirs.forEach(d => common.log(formatter.formatPath(d)));
        } else {
            common.log("\nMatching directories: 0");
        }
    }

    printMatchingFiles(fileResults: FileResult[], formatter: FileResultFormatter): void {
        if (fileResults.length > 0) {
            common.log("\nMatching files " + `(${fileResults.length}):`);
            fileResults.forEach(f => common.log(formatter.formatFileResult(f)));
        } else {
            common.log("\nMatching files: 0");
        }
    }
}
