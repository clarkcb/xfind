/*
 * finder.ts
 *
 * performs the finding based on the given FindSettings instance
 */

'use strict';

import * as assert from 'assert';
import * as fs from 'fs';
import {stat, lstat} from 'fs/promises';
import * as path from 'path';

import {FileResult} from './fileresult';
import {FileResultFormatter} from "./fileresultformatter";
import {FileResultSorter} from "./fileresultsorter";
import {FileType} from './filetype';
import {FileTypes} from './filetypes';
import {FileUtil, ENOENT, EACCES} from './fileutil';
import {FindError} from './finderror';
import {FindSettings} from './findsettings';
import * as common from "./common";

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
                } catch {
                    p = FileUtil.expandPath(p);
                    fs.accessSync(p, fs.constants.F_OK | fs.constants.R_OK);
                }
                let stats = fs.lstatSync(p);
                if (stats.isSymbolicLink()) {
                    assert.ok(this._settings.followSymlinks, startPathDoesNotMatchFindSettings);
                }
                stats = fs.statSync(p);
                if (stats.isDirectory()) {
                    assert.ok(this.isTraversableDirPath(p), startPathDoesNotMatchFindSettings);
                } else if (stats.isFile()) {
                    assert.ok(this.filterToFileResult(p, stats) !== null,
                        startPathDoesNotMatchFindSettings);
                } else {
                    // TODO: start path is unknown/invalid type
                    throw new FindError(startPathDoesNotMatchFindSettings);
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

    public static matchesAnyPattern(s: string, patterns: RegExp[]): boolean {
        return patterns.some((p: RegExp) => s.search(p) > -1);
    }

    public static anyMatchesAnyPattern(ss: string[], patterns: RegExp[]): boolean {
        return ss.some((s: string) => Finder.matchesAnyPattern(s, patterns));
    }

    public static matchesAnyString(s: string, elements: string[]): boolean {
        return elements.indexOf(s) > -1;
    }

    public static matchesAnyFileType(ft: FileType, fileTypes: FileType[]): boolean {
        return fileTypes.indexOf(ft) > -1;
    }

    public static emptyOrMatchesAnyPattern(s: string, patterns: RegExp[]): boolean {
        return patterns.length === 0 || Finder.matchesAnyPattern(s, patterns);
    }

    public static emptyOrNotMatchesAnyPattern(s: string, patterns: RegExp[]): boolean {
        return patterns.length === 0 || !Finder.matchesAnyPattern(s, patterns);
    }

    public static emptyOrAnyMatchesAnyPattern(ss: string[], patterns: RegExp[]): boolean {
        return ss.some((s: string) => Finder.emptyOrMatchesAnyPattern(s, patterns));
    }

    public static emptyOrNotAnyMatchesAnyPattern(ss: string[], patterns: RegExp[]): boolean {
        return ss.some((s: string) => !Finder.emptyOrMatchesAnyPattern(s, patterns));
    }

    public static emptyOrMatchesAnyString(s: string, elements: string[]): boolean {
        return elements.length === 0 || Finder.matchesAnyString(s, elements);
    }

    public static emptyOrNotMatchesAnyString(s: string, elements: string[]): boolean {
        return elements.length === 0 || !Finder.matchesAnyString(s, elements);
    }

    public static emptyOrMatchesAnyFileType(ft: FileType, fileTypes: FileType[]): boolean {
        return fileTypes.length === 0 || Finder.matchesAnyFileType(ft, fileTypes);
    }

    public static emptyOrNotMatchesAnyFileType(ft: FileType, fileTypes: FileType[]): boolean {
        return fileTypes.length === 0 || !Finder.matchesAnyFileType(ft, fileTypes);
    }

    public isMatchingDirPathByHidden(dirPath: string): boolean {
        return this._settings.includeHidden || !FileUtil.isHiddenPath(dirPath);
    }

    public isMatchingDirPathByInPatterns(dirPath: string): boolean {
        const elems = FileUtil.getPathElems(dirPath);
        return Finder.emptyOrAnyMatchesAnyPattern(elems, this._settings.inDirPatterns);
    }

    public isMatchingDirPathByOutPatterns(dirPath: string): boolean {
        const elems = FileUtil.getPathElems(dirPath);
        return Finder.emptyOrNotAnyMatchesAnyPattern(elems, this._settings.outDirPatterns);
    }

    public isTraversableDirPath(dirPath: string): boolean {
        return this.isMatchingDirPathByHidden(dirPath)
            && this.isMatchingDirPathByOutPatterns(dirPath);
    }

    public isMatchingDirPath(dirPath: string): boolean {
        return this.isMatchingDirPathByHidden(dirPath)
            && this.isMatchingDirPathByInPatterns(dirPath)
            && this.isMatchingDirPathByOutPatterns(dirPath);
    }

    public isNullOrMatchingDirPath(dirPath: string): boolean {
        if (!dirPath) return true;
        return this.isMatchingDirPath(dirPath);
    }

    public isMatchingFileNameByHidden(fileName: string): boolean {
        return this._settings.includeHidden || !FileUtil.isHiddenName(fileName);
    }

    public isMatchingArchiveExtension(ext: string): boolean {
        return (Finder.emptyOrMatchesAnyString(ext, this._settings.inArchiveExtensions)
            && Finder.emptyOrNotMatchesAnyString(ext, this._settings.outArchiveExtensions));
    }

    public isMatchingArchiveExtensionForFilePath(filePath: string): boolean {
        if (this._settings.inArchiveExtensions.length || this._settings.outArchiveExtensions.length) {
            const ext: string = FileUtil.getExtension(filePath);
            return this.isMatchingArchiveExtension(ext);
        }
        return true;
    }

    public isMatchingArchiveFileName(fileName: string): boolean {
        return (Finder.emptyOrMatchesAnyPattern(fileName, this._settings.inArchiveFilePatterns)
            && Finder.emptyOrNotMatchesAnyPattern(fileName, this._settings.outArchiveFilePatterns));
    }

    public isMatchingArchiveFileNameForFilePath(filePath: string): boolean {
        if (this._settings.inArchiveFilePatterns.length || this._settings.outArchiveFilePatterns.length) {
            return this.isMatchingArchiveFileName(path.basename(filePath));
        }
        return true;
    }

    public isMatchingArchiveFilePath(filePath: string): boolean {
        return this.isMatchingArchiveExtensionForFilePath(filePath)
            && this.isMatchingArchiveFileNameForFilePath(filePath);
    }

    public isMatchingArchiveFileResult(fr: FileResult): boolean {
        return this.isMatchingArchiveFilePath(fr.filePath);
    }

    public isMatchingExtension(ext: string): boolean {
        return (Finder.emptyOrMatchesAnyString(ext, this._settings.inExtensions)
            && Finder.emptyOrNotMatchesAnyString(ext, this._settings.outExtensions));
    }

    public isMatchingExtensionForFilePath(filePath: string): boolean {
        if (this._settings.inExtensions.length || this._settings.outExtensions.length) {
            const ext: string = FileUtil.getExtension(filePath);
            return this.isMatchingExtension(ext);
        }
        return true;
    }

    public isMatchingFileName(fileName: string): boolean {
        return (Finder.emptyOrMatchesAnyPattern(fileName, this._settings.inFilePatterns)
            && Finder.emptyOrNotMatchesAnyPattern(fileName, this._settings.outFilePatterns));
    }

    public isMatchingFileNameForFilePath(filePath: string): boolean {
        if (this._settings.inFilePatterns.length || this._settings.outFilePatterns.length) {
            return this.isMatchingFileName(path.basename(filePath));
        }
        return true;
    }

    public isMatchingFilePath(filePath: string): boolean {
        return (this.isMatchingExtensionForFilePath(filePath)
            && this.isMatchingFileNameForFilePath(filePath));
    }

    public isMatchingFileType(fileType: FileType): boolean {
        return (Finder.emptyOrMatchesAnyFileType(fileType, this._settings.inFileTypes)
            && Finder.emptyOrNotMatchesAnyFileType(fileType, this._settings.outFileTypes));
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
        return this.isMatchingFilePath(fr.filePath)
            && this.isMatchingFileType(fr.fileType)
            && this.isMatchingFileSize(fr.fileSize)
            && this.isMatchingLastMod(fr.lastMod);
    }

    public filterArchiveFilePathToFileResult(filePath: string): FileResult | null {
        if (!this._settings.includeArchives && !this._settings.archivesOnly) {
            return null;
        }

        if (!this.isMatchingArchiveFilePath(filePath)) {
            return null;
        }

        return new FileResult(filePath, FileType.Archive, 0, 0);
    }

    public filterRegularFilePathToFileResult(filePath: string, fileType: FileType, stat: fs.Stats | null = null): FileResult | null {
        if (this._settings.archivesOnly) {
            return null;
        }

        if (!this.isMatchingFilePath(filePath) || !this.isMatchingFileType(fileType)) {
            return null;
        }

        let fileSize = 0;
        let lastMod = 0;
        if (this._settings.needLastMod() || this._settings.needSize()) {
            stat = stat || fs.statSync(filePath);
            if (this._settings.needSize()) fileSize = stat.size;
            if (this._settings.needLastMod()) lastMod = stat.mtime.getTime();

            if (!this.isMatchingFileSize(fileSize) || !this.isMatchingLastMod(lastMod)) {
                return null;
            }
        }

        return new FileResult(filePath, fileType, fileSize, lastMod);
    }

    public filterToFileResult(filePath: string, stat: fs.Stats | null = null): FileResult | null {
        if (!this.isNullOrMatchingDirPath(path.dirname(filePath))
            || !this.isMatchingFileNameByHidden(path.basename(filePath))) {
            return null;
        }

        const fileType = FileTypes.getFileType(path.basename(filePath));
        if (fileType === FileType.Archive) {
            return this.filterArchiveFilePathToFileResult(filePath);
        }
        return this.filterRegularFilePathToFileResult(filePath, fileType, stat);
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
        const filePaths = fs.readdirSync(currentDir, { recursive: false })
            .map(f =>  path.join(currentDir, f.toString()));
        for (const filePath of filePaths) {
            let stats = fs.lstatSync(filePath);
            if (stats.isSymbolicLink() && !this._settings.followSymlinks) {
                continue;
            }
            stats = fs.statSync(filePath);
            if (stats.isDirectory() && recurse && this.isTraversableDirPath(filePath)) {
                findDirs.push(filePath);
            } else if (stats.isFile() && (minDepth < 0 || currentDepth >= minDepth)) {
                const fr = this.filterToFileResult(filePath, stats);
                if (fr !== null) {
                    fileResults.push(fr);
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
        } catch {
            filePath = FileUtil.expandPath(filePath);
        }
        let stats = await lstat(filePath);
        if (stats.isSymbolicLink() && !this._settings.followSymlinks) {
            throw new FindError(startPathDoesNotMatchFindSettings);
        }
        stats = await stat(filePath);
        if (stats.isDirectory()) {
            // if max_depth is zero, we can skip since a directory cannot be a result
            if (this._settings.maxDepth === 0) {
                return [];
            }
            if (this.isTraversableDirPath(filePath)) {
                let maxDepth = this._settings.maxDepth;
                if (!this._settings.recursive) {
                    maxDepth = 1;
                }
                return await this.recGetFileResults(filePath, this._settings.minDepth, maxDepth, 1);
            } else {
                throw new FindError(startPathDoesNotMatchFindSettings);
            }
        } else if (stats.isFile()) {
            // if min_depth > zero, we can skip since the file is at depth zero
            if (this._settings.minDepth > 0) {
                return [];
            }
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

    public async find(): Promise<FileResult[]> {
        // get the file results
        let fileResults: FileResult[] = [];

        const pathFileResultsArrays =
            await Promise.all(this._settings.paths.map(p => this.getFileResults(p)));
        pathFileResultsArrays.forEach(pathFileResults => {
            fileResults = fileResults.concat(pathFileResults);
        });

        const fileResultSorter = new FileResultSorter(this._settings);
        fileResultSorter.sort(fileResults);
        return fileResults;
    }

    getMatchingDirs(fileResults: FileResult[]): string[] {
        const dirs: string[] = fileResults.map(fr => path.dirname(fr.filePath));
        return common.setFromArray(dirs);
    }

    printMatchingDirs(fileResults: FileResult[], formatter: FileResultFormatter): void {
        const dirs: string[] = this.getMatchingDirs(fileResults);
        if (dirs.length > 0) {
            common.log("\nMatching directories " + `(${dirs.length}):`);
            dirs.forEach(d => common.log(formatter.formatDirPath(d)));
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
