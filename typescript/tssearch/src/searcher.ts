/*
 * searcher.ts
 *
 * performs the searching based on the given SearchSettings instance
 */

"use strict";

const assert = require('assert');
const fs = require('fs');
const path = require('path');

import * as common from './common';
import {FileType} from './filetype';
import {FileTypes} from './filetypes';
import {FileUtil} from './fileutil';
import {SearchFile} from './searchfile';
import {SearchResult} from './searchresult';
import {SearchSettings} from './searchsettings';

export class Searcher {

    _settings: SearchSettings;
    results: SearchResult[] = [];

    constructor(settings: SearchSettings) {
        this._settings = settings;
        this.validateSettings();
    }

    private validateSettings(): void {
        assert.ok(this._settings.startPath, 'Startpath not defined');
        assert.ok(fs.existsSync(this._settings.startPath), 'Startpath not found');
        assert.ok(this.isSearchDir(this._settings.startPath), 'Startpath does not match search settings');
        assert.ok(this._settings.searchPatterns.length, 'No search patterns defined');
        assert.equal(this._settings.textFileEncoding, "utf-8", "Invalid encoding");
    }

    private static matchesAnyElement(s: string, elements: string[]): boolean {
        return elements.indexOf(s) > -1;
    }

    private static matchesAnyPattern(s: string, patterns: RegExp[]): boolean {
        return patterns.some((p: RegExp, i: number, arr) => s.search(p) > -1);
    }

    private static anyMatchesAnyPattern(ss: string[], patterns: RegExp[]) {
        return ss.some((s: string, i: number, arr) => this.matchesAnyPattern(s, patterns));
    }

    public isSearchDir(dir: string): boolean {
        if (FileUtil.isDotDir(dir)) {
            return true;
        }
        if (this._settings.excludeHidden) {
            let nonDotElems = dir.split(path.sep).filter((p: string) => !Searcher.matchesAnyElement(p, ['.','..']));
            if (nonDotElems.length === 0) {
                return true;
            }
            if (nonDotElems.some((p: string, i: number, arr) => FileUtil.isHidden(p))) {
                return false;
            }
        }
        if (this._settings.inDirPatterns.length && !Searcher.matchesAnyPattern(dir,
                this._settings.inDirPatterns)) {
            return false;
        }
        return !(this._settings.outDirPatterns.length && Searcher.matchesAnyPattern(dir,
            this._settings.outDirPatterns));
    }

    public isSearchFile(file: string): boolean {
        if (FileUtil.isHidden(file) && this._settings.excludeHidden) {
            return false;
        }
        let ext: string = FileUtil.getExtension(file);
        if (this._settings.inExtensions.length &&
            !Searcher.matchesAnyElement(ext, this._settings.inExtensions)) {
            return false;
        }
        if (this._settings.outExtensions.length &&
            Searcher.matchesAnyElement(ext, this._settings.outExtensions)) {
            return false;
        }
        if (this._settings.inFilePatterns.length &&
            !Searcher.matchesAnyPattern(file, this._settings.inFilePatterns)) {
            return false;
        }
        if (this._settings.outFilePatterns.length &&
            Searcher.matchesAnyPattern(file, this._settings.outFilePatterns)) {
            return false;
        }
        let filetype: FileType = FileTypes.getFileType(file);
        if (this._settings.inFileTypes.length &&
            !Searcher.matchesAnyElement(filetype, this._settings.inFileTypes)) {
            return false;
        }
        if (this._settings.outFileTypes.length &&
            Searcher.matchesAnyElement(filetype, this._settings.outFileTypes)) {
            return false;
        }
        return true;
    }

    public isArchiveSearchFile(file: string): boolean {
        if (FileUtil.isHidden(file) && this._settings.excludeHidden) {
            return false;
        }
        let ext: string = FileUtil.getExtension(file);
        if (this._settings.inArchiveExtensions.length &&
            !Searcher.matchesAnyElement(ext, this._settings.inArchiveExtensions)) {
            return false;
        }
        if (this._settings.outArchiveExtensions.length &&
            Searcher.matchesAnyElement(ext, this._settings.outArchiveExtensions)) {
            return false;
        }
        if (this._settings.inArchiveFilePatterns.length &&
            !Searcher.matchesAnyPattern(file, this._settings.inArchiveFilePatterns)) {
            return false;
        }
        return !(this._settings.outArchiveFilePatterns.length &&
        Searcher.matchesAnyPattern(file, this._settings.outArchiveFilePatterns));
    }

    private getSearchDirs(startPath: string): string[] {
        let searchDirs: string[] = [];
        let stats = fs.statSync(startPath);
        if (stats.isDirectory()) {
            if (this.isSearchDir(startPath)) {
                searchDirs.push(startPath);
                if (this._settings.recursive) {
                    if (this._settings.debug) {
                        common.log(`Getting list of directories to search under ${startPath}`);
                    }
                    [].push.apply(searchDirs, this.recGetSearchDirs(startPath));
                }
            } else {
                common.log("Warning: startPath does not match search criteria");
            }
        } else if (stats.isFile()) {
            let d: string = path.dirname(startPath);
            if (!d) d = ".";
            if (this.isSearchDir(d)) {
                searchDirs.push(d);
            } else {
                common.log("Warning: startPath's path does not match search criteria");
            }
        }
        return searchDirs;
    }

    private recGetSearchDirs(currentDir: string): string[] {
        let self = this;
        let searchDirs: string[] = [];
        fs.readdirSync(currentDir).map(function (f) {
            return path.join(currentDir, f);
        }).filter(function (f) {
            return fs.statSync(f).isDirectory() && self.isSearchDir(f);
        }).forEach(function (f) {
            searchDirs.push(f);
            [].push.apply(searchDirs, self.recGetSearchDirs(f));
        });
        return searchDirs;
    }

    //private static handleFsError(err): void {
    //    if (err.errno === 34 && err.code === "ENOENT") {
    //        // this error seems to occur when the file is a soft link
    //        // to a non-existent file
    //    } else {
    //        common.log(err);
    //        process.exit(1);
    //    }
    //}

    public filterFile(f: string): boolean {
        if (FileTypes.isArchiveFile(f)) {
            return (this._settings.searchArchives && this.isArchiveSearchFile(f));
        }
        return (!this._settings.archivesOnly && this.isSearchFile(f));
    }

    private getSearchFilesForDirectory(dir: string): string[] {
        let self = this;
        return fs.readdirSync(dir).map(function (f) {
            return path.join(dir, f);
        }).filter(function (f) {
            return fs.statSync(f).isFile() && self.filterFile(f);
        });
    }

    private getSearchFiles(searchDirs: string[]): string[] {
        let searchFiles: string[] = [];
        let stats = fs.statSync(this._settings.startPath);
        if (stats.isDirectory()) {
            let self = this;
            searchDirs.forEach(function(d) {
                searchFiles.push.apply(searchFiles, self.getSearchFilesForDirectory(d));
            });
        } else if (stats.isFile()) {
            searchFiles.push(this._settings.startPath);
        }
        return searchFiles;
    }

    public search() {
        let self = this;
        if (this._settings.verbose) {
            common.log("Search initiated");
        }

        // get the search dirs
        let dirs: string[] = [];
        dirs.push.apply(dirs, this.getSearchDirs(this._settings.startPath));
        if (this._settings.verbose) {
            common.log("\nDirectories to be searched " + `(${dirs.length}):`);
            dirs.forEach(d => common.log(d));
        }

        // get the search files
        let files: string[] = this.getSearchFiles(dirs);
        if (this._settings.verbose) {
            common.log("\nFiles to be searched " + `(${files.length}):`);
            files.forEach(f => common.log(f));
            common.log("");
        }

        // search the files
        files.forEach(f => self.searchFile(f));

        if (this._settings.verbose)
            common.log("Search complete.");
    }

    private searchFile(filepath: string): void {
        let filetype: FileType = FileTypes.getFileType(filepath);
        if (filetype === FileType.Text) {
            this.searchTextFile(filepath);
        } else if (filetype === FileType.Binary) {
            this.searchBinaryFile(filepath);
        }
    }

    private searchBinaryFile(filepath: string): void {
        let self = this;
        if (this._settings.verbose) {
            common.log(`Searching binary file: "${filepath}"`);
        }
        let contents: string = FileUtil.getFileContents(filepath);
        let pattern: RegExp;
        let patternResults = {};
        this._settings.searchPatterns.forEach(function(p: RegExp) {
            pattern = new RegExp(p.source, "g");
            if (self._settings.firstMatch && (pattern.source in patternResults)) {
                return;
            }
            let match = pattern.exec(contents);
            while (match) {
                self.addSearchResult(new SearchResult(
                    pattern,
                    filepath,
                    0,
                    match.index+1,
                    pattern.lastIndex+1,
                    null,
                    [],
                    []));
                if (self._settings.firstMatch) {
                    patternResults[pattern.source] = 1;
                    break;
                }
                match = pattern.exec(contents);
            }
        });
    }

    private searchTextFile(filepath: string): void {
        if (this._settings.verbose) {
            common.log(`Searching text file ${filepath}`);
        }
        if (this._settings.multilineSearch)
            this.searchTextFileContents(filepath);
        else
            this.searchTextFileLines(filepath);
    }

    private searchTextFileContents(filepath: string): void {
        let self = this;
        let contents: string = FileUtil.getFileContents(filepath);
        let results: SearchResult[] = this.searchMultiLineString(contents);
        results.forEach(function(r: SearchResult) {
            let resultWithFilepath: SearchResult =
                new SearchResult(r.pattern, filepath, r.linenum,
                    r.matchStartIndex, r.matchEndIndex, r.line,
                    r.linesBefore, r.linesAfter);
            self.addSearchResult(resultWithFilepath);
        });
    }

    private static getNewLineIndices(s: string): number[] {
        let indices: number[] = [];
        for (let i = 0; i < s.length; i++) {
            if (s.charAt(i) == "\n") {
                indices.push(i);
            }
        }
        return indices;
    }

    private static getLinesAtIndices(s: string, atIndices: number[],
                                     startLineIndices: number[],
                                     endLineIndices: number[]) {
        if (atIndices.length === 0)
            return [];
        let lines: string[] = [];
        atIndices.forEach(function(i: number): void {
            let line: string = s.substring(i, endLineIndices[startLineIndices.indexOf(i)]);
            lines.push(line);
        });
        return lines;
    }

    private static getLinesBefore(s: string, beforeStartIndices: number[],
                                  startLineIndices: number[],
                                  endLineIndices: number[]) {
        return Searcher.getLinesAtIndices(s, beforeStartIndices,
            startLineIndices, endLineIndices);
    }

    private static getLinesAfter(s: string, afterStartIndices: number[],
                                 startLineIndices: number[],
                                 endLineIndices: number[]) {
        return Searcher.getLinesAtIndices(s, afterStartIndices,
            startLineIndices, endLineIndices);
    }

    private getLessThanOrEqual(matchIdx: number) {
        return function(i: number): boolean { return i <= matchIdx; };
    }

    private getGreaterThan(matchIdx: number) {
        return function(i: number): boolean { return i > matchIdx; };
    }

    public searchMultiLineString(s: string): SearchResult[] {
        let self = this;
        let patternResults = {};
        let linesBefore: string[] = [];
        let linesAfter: string[] = [];
        let results: SearchResult[] = [];
        let newLineIndices: number[] = Searcher.getNewLineIndices(s);
        let plusOne = function(i: number): number { return i+1; };
        let startLineIndices: number[] = [0].concat(newLineIndices.map(plusOne));
        let endLineIndices: number[] = newLineIndices.concat([s.length - 1]);

        this._settings.searchPatterns.forEach(function(p: RegExp) {
            let pattern: RegExp = new RegExp(p.source, "g");
            let match = pattern.exec(s);
            let stop: boolean = false;
            while (match && !stop) {
                if (self._settings.firstMatch && pattern.source in patternResults) {
                    stop = true;
                    continue;
                }
                let lessOrEqual = self.getLessThanOrEqual(match.index);
                let greaterThan = self.getGreaterThan(match.index);
                let lineStartIndex: number = 0;
                let lineEndIndex: number = s.length - 1;
                let beforeLineCount: number = 0;
                let beforeStartIndices: number[] = startLineIndices.filter(lessOrEqual);
                if (beforeStartIndices.length > 0) {
                    lineStartIndex = beforeStartIndices.pop();
                    beforeLineCount = beforeStartIndices.length;
                    if (beforeStartIndices.length > self._settings.linesBefore) {
                        beforeStartIndices = beforeStartIndices.slice(
                            beforeStartIndices.length - self._settings.linesBefore);
                    }
                }
                lineEndIndex = endLineIndices[startLineIndices.indexOf(lineStartIndex)];
                let line: string = s.substring(lineStartIndex, lineEndIndex);
                if (self._settings.linesBefore && beforeLineCount) {
                    linesBefore = Searcher.getLinesBefore(s, beforeStartIndices,
                        startLineIndices, endLineIndices);
                }
                if (self._settings.linesAfter) {
                    let afterStartIndices: number[] = startLineIndices.filter(greaterThan);
                    if (afterStartIndices.length > self._settings.linesAfter) {
                        afterStartIndices = afterStartIndices.slice(0,
                            self._settings.linesAfter);
                    }
                    linesAfter = Searcher.getLinesAfter(s, afterStartIndices,
                        startLineIndices, endLineIndices);
                }
                let matchStartIndex: number = match.index - lineStartIndex + 1;
                let matchEndIndex: number = pattern.lastIndex - lineStartIndex + 1;
                if ((self._settings.linesBefore === 0 || self.linesBeforeMatch(linesBefore)) &&
                    (self._settings.linesAfter === 0 || self.linesAfterMatch(linesAfter))) {
                    let searchResult: SearchResult = new SearchResult(
                        pattern,
                        '',
                        beforeLineCount+1,
                        matchStartIndex,
                        matchEndIndex,
                        line,
                        [].concat(linesBefore),
                        [].concat(linesAfter));
                    results.push(searchResult);
                    if (!(pattern.source in patternResults)) {
                        patternResults[pattern.source] = 1;
                    }
                }
                match = pattern.exec(s);
            }
        });
        return results;
    }

    private static linesMatch(lines: string[], inPatterns: RegExp[], outPatterns: RegExp[]): boolean {
        return ((inPatterns.length === 0 || Searcher.anyMatchesAnyPattern(lines, inPatterns)) &&
               (outPatterns.length === 0 || ! Searcher.anyMatchesAnyPattern(lines, outPatterns)));
    }

    private linesBeforeMatch(linesBefore: string[]): boolean {
        return Searcher.linesMatch(linesBefore, this._settings.inLinesBeforePatterns,
            this._settings.outLinesBeforePatterns);
    }

    private linesAfterMatch(linesAfter: string[]): boolean {
        return Searcher.linesMatch(linesAfter, this._settings.inLinesAfterPatterns,
            this._settings.outLinesAfterPatterns);
    }

    private searchTextFileLines(filepath: string): void {
        let self = this;
        let lines: string[] = FileUtil.getFileLines(filepath);
        let results: SearchResult[] = this.searchLines(lines);
        results.forEach(function(r: SearchResult) {
            let resultWithFilepath: SearchResult =
                new SearchResult(r.pattern, filepath, r.linenum,
                    r.matchStartIndex, r.matchEndIndex, r.line,
                    r.linesBefore, r.linesAfter);
            self.addSearchResult(resultWithFilepath);
        });
    }

    // return results so that filepath can be added to them
    public searchLines(lines: string[]): SearchResult[] {
        let self = this;
        let linenum: number = 0;
        let pattern: RegExp;
        let linesBefore: string[] = [];
        let linesAfter: string[] = [];
        let results: SearchResult[] = [];
        let patternResults = {};
        while (true) {
            if (Object.keys(patternResults).length === this._settings.searchPatterns.length) {
                break;
            }
            let line: string = "";
            if (linesAfter.length > 0) {
                line = linesAfter.shift();
            } else if (lines.length > 0) {
                line = lines.shift();
            } else {
                break;
            }
            linenum += 1;
            if (this._settings.linesAfter > 0) {
                while (linesAfter.length < this._settings.linesAfter && lines.length > 0) {
                    linesAfter.push(lines.shift());
                }
            }
            this._settings.searchPatterns.forEach(function(p: RegExp) {
                pattern = new RegExp(p.source, "g");
                let match = pattern.exec(line);
                while (match) {
                    if ((self._settings.linesBefore === 0 || self.linesBeforeMatch(linesBefore)) &&
                        (self._settings.linesAfter === 0 || self.linesAfterMatch(linesAfter))) {
                        results.push(new SearchResult(
                            pattern,
                            '',
                            linenum,
                            match.index+1,
                            pattern.lastIndex+1,
                            line,
                            [].concat(linesBefore),
                            [].concat(linesAfter)));
                        if (self._settings.firstMatch) {
                            patternResults[pattern.source] = 1;
                            break;
                        }
                    }
                    match = pattern.exec(line);
                }
            });
            if (this._settings.linesBefore > 0) {
                if (linesBefore.length == this._settings.linesBefore)
                    linesBefore.shift();
                if (linesBefore.length < this._settings.linesBefore)
                    linesBefore.push(line);
            }
        }
        return results;
    }

    private addSearchResult(result: SearchResult): void {
        this.results.push(result);
    }

    private cmpSearchResults(r1: SearchResult, r2: SearchResult): number {
        const pathCmp: number = path.dirname(r1.filename).localeCompare(path.dirname(r2.filename));
        if (pathCmp === 0) {
            const fileCmp: number = path.basename(r1.filename).localeCompare(path.basename(r2.filename));
            if (fileCmp === 0) {
                if (r1.linenum === r2.linenum) {
                    return r1.matchStartIndex - r2.matchStartIndex;
                }
                return r1.linenum - r2.linenum;
            }
            return fileCmp;
        }
        return pathCmp;
    }

    public printSearchResults(): void {
        // first sort the results
        this.results.sort(this.cmpSearchResults);
        common.log("\nSearch results " + `(${this.results.length}):`);
        this.results.forEach(r => common.log(r.toString()));
    }

    public getMatchingDirs(): string[] {
        const dirs: string[] = this.results.map(r =>path.dirname(r.filename));
        return common.setFromArray(dirs);
    }

    public printMatchingDirs(): void {
        const dirs: string[] = this.getMatchingDirs();
        common.log("\nDirectories with matches " + `(${dirs.length}):`);
        dirs.forEach(d => common.log(d));
    }

    public getMatchingFiles(): string[] {
        const files: string[] = this.results.map(r => r.filename);
        return common.setFromArray(files);
    }

    public printMatchingFiles(): void {
        const files: string[] = this.getMatchingFiles();
        common.log("\nFiles with matches " + `(${files.length}):`);
        files.forEach(f => common.log(f));
    }

    public getMatchingLines(): string[] {
        let lines: string[] = this.results.filter(r => r.linenum > 0).map(r => r.line.trim());
        if (this._settings.uniqueLines) {
            lines = common.setFromArray(lines);
        }
        lines.sort((a, b) => {
            if (a.toUpperCase() === b.toUpperCase())
                return 0;
            return a.toUpperCase() < b.toUpperCase() ? -1 : 1;
        });
        return lines;
    }

    public printMatchingLines(): void {
        const lines: string[] = this.getMatchingLines();
        let hdrText: string;
        if (this._settings.uniqueLines)
            hdrText = "\nUnique lines with matches " + `(${lines.length}):`;
        else
            hdrText = "\nLines with matches " + `(${lines.length}):`;
        common.log(hdrText);
        lines.forEach(l => common.log(l));
    }
}
