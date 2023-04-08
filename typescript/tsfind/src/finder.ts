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
                if (stat.isDirectory()) {
                    assert.ok(this.isMatchingDir(p),
                        'Startpath does not match find settings');
                } else if (stat.isFile()) {
                    assert.ok(this.filterFile(p),
                        'Startpath does not match find settings');
                } else {
                    assert.ok(false, 'Startpath not findable file type');
                }
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

    private static anyMatchesAnyPattern(ss: string[], patterns: RegExp[]) {
        return ss.some((s: string) => this.matchesAnyPattern(s, patterns));
    }

    public isMatchingDir(dir: string): boolean {
        if (FileUtil.isDotDir(dir)) {
            return true;
        }
        if (this._settings.excludeHidden) {
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

    public isMatchingFile(file: string): boolean {
        // if (FileUtil.isHidden(file) && this._settings.excludeHidden) {
        //     return false;
        // }
        if (this._settings.inExtensions.length || this._settings.outExtensions.length) {
            const ext: string = FileUtil.getExtension(file);
            if ((this._settings.inExtensions.length &&
                    !Finder.matchesAnyString(ext, this._settings.inExtensions))
                || (this._settings.outExtensions.length &&
                    Finder.matchesAnyString(ext, this._settings.outExtensions))) {
                return false;
            }
        }
        if ((this._settings.inFilePatterns.length &&
            !Finder.matchesAnyPattern(file, this._settings.inFilePatterns))
            || (this._settings.outFilePatterns.length &&
                Finder.matchesAnyPattern(file, this._settings.outFilePatterns))) {
            return false;
        }
        const filetype: FileType = FileTypes.getFileType(file);
        if ((this._settings.inFileTypes.length &&
            !Finder.matchesAnyFileType(filetype, this._settings.inFileTypes))
            || (this._settings.outFileTypes.length &&
                Finder.matchesAnyFileType(filetype, this._settings.outFileTypes))) {
                    return false;
        }
        return true;
    }

    public isMatchingFileResult(fr: FileResult): boolean {
        // if (FileUtil.isHidden(file) && this._settings.excludeHidden) {
        //     return false;
        // }
        if (this._settings.inExtensions.length || this._settings.outExtensions.length) {
            const ext: string = FileUtil.getExtension(fr.filename);
            if ((this._settings.inExtensions.length &&
                    !Finder.matchesAnyString(ext, this._settings.inExtensions))
                || (this._settings.outExtensions.length &&
                    Finder.matchesAnyString(ext, this._settings.outExtensions))) {
                return false;
            }
        }
        if ((this._settings.inFilePatterns.length &&
            !Finder.matchesAnyPattern(fr.filename, this._settings.inFilePatterns))
            || (this._settings.outFilePatterns.length &&
                Finder.matchesAnyPattern(fr.filename, this._settings.outFilePatterns))) {
            return false;
        }
        if ((this._settings.inFileTypes.length &&
            !Finder.matchesAnyFileType(fr.filetype, this._settings.inFileTypes))
            || (this._settings.outFileTypes.length &&
                Finder.matchesAnyFileType(fr.filetype, this._settings.outFileTypes))) {
                    return false;
        }
        if (fr.stat !== null) {
            if ((this._settings.maxLastMod !== null && fr.stat.mtime.getTime() > this._settings.maxLastMod.getTime()) ||
                (this._settings.minLastMod !== null && fr.stat.mtime.getTime() < this._settings.minLastMod.getTime())) {
                return false;
            }
            if ((this._settings.maxSize > 0 && fr.stat.size > this._settings.maxSize) ||
                (this._settings.minSize > 0 && fr.stat.size < this._settings.minSize)) {
                return false;
            }
        }
        return true;
    }

    public isMatchingArchiveFile(file: string): boolean {
        // if (FileUtil.isHidden(file) && this._settings.excludeHidden) {
        //     return false;
        // }
        const ext: string = FileUtil.getExtension(file);
        if (this._settings.inArchiveExtensions.length &&
            !Finder.matchesAnyString(ext, this._settings.inArchiveExtensions)) {
            return false;
        }
        if (this._settings.outArchiveExtensions.length &&
            Finder.matchesAnyString(ext, this._settings.outArchiveExtensions)) {
            return false;
        }
        if (this._settings.inArchiveFilePatterns.length &&
            !Finder.matchesAnyPattern(file, this._settings.inArchiveFilePatterns)) {
            return false;
        }
        return !(this._settings.outArchiveFilePatterns.length &&
        Finder.matchesAnyPattern(file, this._settings.outArchiveFilePatterns));
    }

    public filterFile(f: string): boolean {
        if (this._settings.excludeHidden && FileUtil.isHidden(f)) {
            return false;
        }
        if (FileTypes.isArchiveFile(f)) {
            return (this._settings.includeArchives && this.isMatchingArchiveFile(f));
        }
        return (!this._settings.archivesOnly && this.isMatchingFile(f));
    }

    public filterToFileResult(fp: string): FileResult | null {
        if (this._settings.excludeHidden && FileUtil.isHidden(fp)) {
            return null;
        }
        const dirname = path.dirname(fp) || '.';
        const filename = path.basename(fp);
        let stat: fs.Stats | null = null;
        if (this._settings.needStat()) {
            stat = fs.statSync(fp);
        }
        const fr = new FileResult(dirname, filename, FileTypes.getFileType(filename), stat);
        if (fr.filetype === FileType.Archive) {
            if (this._settings.includeArchives && this.isMatchingArchiveFile(fr.filename)) {
                return fr;
            }
            return null;
        }
        if (!this._settings.archivesOnly && this.isMatchingFileResult(fr)) {
            return fr;
        }
        return null;
    }

    private recGetFileResults(currentDir: string): FileResult[] {
        const findDirs: string[] = [];
        let fileResults: FileResult[] = [];
        fs.readdirSync(currentDir).map((f: string) => {
            return path.join(currentDir, f);
        }).forEach((fp: string) => {
            const stats = fs.statSync(fp);
            if (stats.isDirectory() && this._settings.recursive && this.isMatchingDir(fp)) {
                findDirs.push(fp);
            } else if (stats.isFile()) {
                // const dirname = path.dirname(f) || '.';
                // const filename = path.basename(f);
                // if (this.filterFile(filename)) {
                //     const filetype = FileTypes.getFileType(filename);
                //     const fr = new FileResult(dirname, filename, filetype);
                //     fileResults.push(fr);
                // }
                const fr = this.filterToFileResult(fp);
                if (fr !== null) {
                    fileResults.push(fr);
                }
            }
        });
        findDirs.forEach(d => {
            fileResults = fileResults.concat(this.recGetFileResults(d));
        });
        return fileResults;
    }

    private async getFileResults(startPath: string): Promise<FileResult[]> {
        let fileResults: FileResult[] = [];
        const stats = await stat(startPath);
        if (stats.isDirectory()) {
            if (this.isMatchingDir(startPath)) {
                fileResults = fileResults.concat(this.recGetFileResults(startPath));
            } else {
                throw new FindError('startPath does not match find criteria');
            }
        } else if (stats.isFile()) {
            const dirname = path.dirname(startPath) || '.';
            if (this.isMatchingDir(dirname)) {
                const fr = this.filterToFileResult(startPath);
                if (fr !== null) {
                    fileResults.push(fr);
                } else {
                    throw new FindError('startPath does not match find criteria');
                }
            } else {
                throw new FindError('startPath does not match find criteria');
            }
        }
        return fileResults;
    }

    private cmpFileResultsByPath(fr1: FileResult, fr2: FileResult): number {
        const [path1, path2]: string[] = this._settings.sortCaseInsensitive ?
            [fr1.pathname.toLowerCase(), fr2.pathname.toLowerCase()] :
            [fr1.pathname, fr2.pathname];
        if (path1 === path2) {
            const [filename1, filename2]: string[] = this._settings.sortCaseInsensitive ?
                [fr1.filename.toLowerCase(), fr2.filename.toLowerCase()] :
                [fr1.filename, fr2.filename];
            return filename1 < filename2 ? -1 : 1;
        }
        return path1 < path2 ? -1 : 1;
    }

    private cmpFileResultsByName(fr1: FileResult, fr2: FileResult): number {
        const [filename1, filename2]: string[] = this._settings.sortCaseInsensitive ?
            [fr1.filename.toLowerCase(), fr2.filename.toLowerCase()] :
            [fr1.filename, fr2.filename];
        if (filename1 === filename2) {
            const [path1, path2]: string[] = this._settings.sortCaseInsensitive ?
                [fr1.pathname.toLowerCase(), fr2.pathname.toLowerCase()] :
                [fr1.pathname, fr2.pathname];
            return path1 < path2 ? -1 : 1;
        }
        return filename1 < filename2 ? -1 : 1;
    }

    private cmpFileResultsBySize(fr1: FileResult, fr2: FileResult): number {
        if (fr1.stat !== null && fr2.stat !== null) {
            if (fr1.stat.size === fr2.stat.size) {
                return this.cmpFileResultsByPath(fr1, fr2);
            }
            return fr1.stat.size - fr2.stat.size;
        }
        return 0;
    }

    private cmpFileResultsByType(fr1: FileResult, fr2: FileResult): number {
        if (fr1.filetype === fr2.filetype) {
            return this.cmpFileResultsByPath(fr1, fr2);
        }
        return fr1.filetype - fr2.filetype;
    }

    private cmpFileResultsByLastMod(fr1: FileResult, fr2: FileResult): number {
        if (fr1.stat !== null && fr2.stat !== null) {
            if (fr1.stat.mtime.getTime() === fr2.stat.mtime.getTime()) {
                return this.cmpFileResultsByPath(fr1, fr2);
            }
            return fr1.stat.mtime.getTime() - fr2.stat.mtime.getTime();
        }
        return 0;
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
        const dirs: string[] = fileResults.map(f => f.pathname);
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
