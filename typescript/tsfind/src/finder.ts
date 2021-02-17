/*
 * finder.ts
 *
 * performs the finding based on the given FindSettings instance
 */

"use strict";

const assert = require('assert');
const fs = require('fs');
const path = require('path');

import * as common from './common';
import {FileType} from './filetype';
import {FileTypes} from './filetypes';
import {FileUtil} from './fileutil';
import {FindError} from './finderror';
import {FindFile} from './findfile';
import {FindResult} from './findresult';
import {FindResultFormatter} from './findresultformatter';
import {FindSettings} from './findsettings';

export class Finder {
    _binaryEncoding = 'latin1';
    // from https://github.com/nodejs/node/blob/master/lib/buffer.js
    _supportedEncodings: string[] = ['utf-8', 'utf8', 'latin1', 'ascii', 'ucs2',  'ucs-2', 'utf16le',
        'binary', 'base64', 'hex'];

    _settings: FindSettings;
    results: FindResult[] = [];

    constructor(settings: FindSettings) {
        this._settings = settings;
        this.validateSettings();
    }

    private validateSettings(): void {
        try {
            assert.ok(!!this._settings.startPath, 'Startpath not defined');

            fs.accessSync(this._settings.startPath, fs.constants.F_OK | fs.constants.R_OK);

            const stat = fs.lstatSync(this._settings.startPath);

            if (stat.isDirectory()) {
                assert.ok(this.isFindDir(this._settings.startPath),
                    'Startpath does not match find settings');
            } else if (stat.isFile()) {
                assert.ok(this.filterFile(this._settings.startPath),
                    'Startpath does not match find settings');
            } else {
                assert.ok(false, 'Startpath not findable file type');
            }
            assert.ok(this._settings.findPatterns.length, 'No find patterns defined');
            assert.ok(this._supportedEncodings.indexOf(this._settings.textFileEncoding) > -1,
                'Invalid encoding');
            assert.ok(this._settings.linesBefore > -1, 'Invalid linesbefore');
            assert.ok(this._settings.linesAfter > -1, 'Invalid linesafter');
            assert.ok(this._settings.maxLineLength > -1, 'Invalid maxlinelength');

        } catch (err) {
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
        return patterns.some((p: RegExp) => s.find(p) > -1);
    }

    private static matchesAnyFileType(ft: FileType, fileTypes: FileType[]): boolean {
        return fileTypes.indexOf(ft) > -1;
    }

    private static anyMatchesAnyPattern(ss: string[], patterns: RegExp[]) {
        return ss.some((s: string) => this.matchesAnyPattern(s, patterns));
    }

    public isFindDir(dir: string): boolean {
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

    public isFindFile(file: string): boolean {
        if (FileUtil.isHidden(file) && this._settings.excludeHidden) {
            return false;
        }
        const ext: string = FileUtil.getExtension(file);
        if ((this._settings.inExtensions.length &&
            !Finder.matchesAnyString(ext, this._settings.inExtensions))
            || (this._settings.outExtensions.length &&
                Finder.matchesAnyString(ext, this._settings.outExtensions))
            || (this._settings.inFilePatterns.length &&
                !Finder.matchesAnyPattern(file, this._settings.inFilePatterns))
            || (this._settings.outFilePatterns.length &&
                Finder.matchesAnyPattern(file, this._settings.outFilePatterns))) {
            return false;
        }
        const filetype: FileType = FileTypes.getFileType(file);
        return !((this._settings.inFileTypes.length &&
            !Finder.matchesAnyFileType(filetype, this._settings.inFileTypes))
            || (this._settings.outFileTypes.length &&
                Finder.matchesAnyFileType(filetype, this._settings.outFileTypes)));
    }

    public isArchiveFindFile(file: string): boolean {
        if (FileUtil.isHidden(file) && this._settings.excludeHidden) {
            return false;
        }
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
        if (FileTypes.isArchiveFile(f)) {
            return (this._settings.findArchives && this.isArchiveFindFile(f));
        }
        return (!this._settings.archivesOnly && this.isFindFile(f));
    }

    private getFindFiles(startPath: string): FindFile[] {
        let findFiles: FindFile[] = [];
        const stats = fs.statSync(startPath);
        if (stats.isDirectory()) {
            if (this.isFindDir(startPath)) {
                findFiles = findFiles.concat(this.recGetFindFiles(startPath));
            } else {
                throw new FindError("startPath does not match find criteria");
            }
        } else if (stats.isFile()) {
            const dirname = path.dirname(startPath) || '.';
            const filename = path.basename(startPath);
            if (this.isFindDir(dirname) && this.filterFile(filename)) {
                const filetype = FileTypes.getFileType(filename);
                const sf = new FindFile(dirname, filename, filetype);
                findFiles.push(sf);
            } else {
                throw new FindError("startPath does not match find criteria");
            }
        }
        return findFiles;
    }

    private recGetFindFiles(currentDir: string): FindFile[] {
        const findDirs: string[] = [];
        let findFiles: FindFile[] = [];
        fs.readdirSync(currentDir).map((f: string) => {
            return path.join(currentDir, f);
        }).forEach((f: string) => {
            const stats = fs.statSync(f);
            if (stats.isDirectory() && this._settings.recursive && this.isFindDir(f)) {
                findDirs.push(f);
            } else if (stats.isFile() && this.filterFile(f)) {
                const dirname = path.dirname(f) || '.';
                const filename = path.basename(f);
                const filetype = FileTypes.getFileType(filename);
                const sf = new FindFile(dirname, filename, filetype);
                findFiles.push(sf);
            }
        });
        findDirs.forEach(d => {
            findFiles = findFiles.concat(this.recGetFindFiles(d));
        });
        return findFiles;
    }

    public find() {
        // get the find files
        const findfiles: FindFile[] = this.getFindFiles(this._settings.startPath);

        if (this._settings.verbose) {
            let dirs = findfiles.map(sf => sf.pathname);
            dirs = common.setFromArray(dirs);
            dirs.sort();
            common.log("\nDirectories to be found " + `(${dirs.length}):`);
            dirs.forEach(d => common.log(d));

            common.log("\nFiles to be found " + `(${findfiles.length}):`);
            findfiles.forEach(sf => common.log(sf.toString()));
            common.log("");
        }

        // find the files
        findfiles.forEach(sf => this.findFile(sf));

        if (this._settings.verbose)
            common.log('Find complete.');
    }

    private findFile(findfile: FindFile): void {
        switch (findfile.filetype) {
            case FileType.Code:
            case FileType.Xml:
            case FileType.Text:
                this.findTextFile(findfile);
                break;
            case FileType.Binary:
                this.findBinaryFile(findfile);
                break;
            default:
                // TODO: add message about unsupported filetype
                break;
        }
    }

    private findBinaryFile(findfile: FindFile): void {
        const self = this;
        if (this._settings.verbose) {
            common.log(`Finding binary file: "${findfile}"`);
        }
        const contents: string = FileUtil.getFileContents(findfile.relativePath(), this._binaryEncoding);
        let pattern: RegExp;
        const patternResults: {[index: string]:number} = {};
        this._settings.findPatterns.forEach(function(p: RegExp) {
            pattern = new RegExp(p.source, 'g');
            if (self._settings.firstMatch && (pattern.source in patternResults)) {
                return;
            }
            let match = pattern.exec(contents);
            while (match) {
                const r = new FindResult(
                    pattern,
                    0,
                    match.index + 1,
                    pattern.lastIndex + 1,
                    '',
                    [],
                    []);
                r.file = findfile;
                self.addFindResult(r);
                if (self._settings.firstMatch) {
                    patternResults[pattern.source] = 1;
                    break;
                }
                match = pattern.exec(contents);
            }
        });
    }

    private findTextFile(findfile: FindFile): void {
        if (this._settings.verbose) {
            common.log(`Finding text file ${findfile}`);
        }
        if (this._settings.multilineFind)
            this.findTextFileContents(findfile);
        else
            this.findTextFileLines(findfile);
    }

    private findTextFileContents(findfile: FindFile): void {
        const self = this;
        const contents: string = FileUtil.getFileContents(findfile.relativePath(), this._settings.textFileEncoding);
        const results: FindResult[] = this.findMultiLineString(contents);
        results.forEach(function(r: FindResult) {
            r.file = findfile;
            self.addFindResult(r);
        });
    }

    private static getNewLineIndices(s: string): number[] {
        const indices: number[] = [];
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
        const lines: string[] = [];
        atIndices.forEach(function(i: number): void {
            const line: string = s.substring(i, endLineIndices[startLineIndices.indexOf(i)]);
            lines.push(line);
        });
        return lines;
    }

    private static getLinesBefore(s: string, beforeStartIndices: number[],
                                  startLineIndices: number[],
                                  endLineIndices: number[]) {
        return Finder.getLinesAtIndices(s, beforeStartIndices,
            startLineIndices, endLineIndices);
    }

    private static getLinesAfter(s: string, afterStartIndices: number[],
                                 startLineIndices: number[],
                                 endLineIndices: number[]) {
        return Finder.getLinesAtIndices(s, afterStartIndices,
            startLineIndices, endLineIndices);
    }

    private getLessThanOrEqual(matchIdx: number) {
        return function(i: number): boolean { return i <= matchIdx; };
    }

    private getGreaterThan(matchIdx: number) {
        return function(i: number): boolean { return i > matchIdx; };
    }

    public findMultiLineString(s: string): FindResult[] {
        const self = this;
        const patternResults: {[index: string]:number} = {};
        let linesBefore: string[] = [];
        let linesAfter: string[] = [];
        const results: FindResult[] = [];
        const newLineIndices: number[] = Finder.getNewLineIndices(s);
        const plusOne = function(i: number): number { return i+1; };
        const startLineIndices: number[] = [0].concat(newLineIndices.map(plusOne));
        const endLineIndices: number[] = newLineIndices.concat([s.length - 1]);

        this._settings.findPatterns.forEach(function(p: RegExp) {
            const pattern = new RegExp(p.source, "g");
            let match = pattern.exec(s);
            let stop = false;
            while (match && !stop) {
                if (self._settings.firstMatch && pattern.source in patternResults) {
                    stop = true;
                    continue;
                }
                const lessOrEqual = self.getLessThanOrEqual(match.index);
                const greaterThan = self.getGreaterThan(match.index);
                let lineStartIndex = 0;
                let lineEndIndex = s.length - 1;
                let beforeLineCount = 0;
                let beforeStartIndices: number[] = startLineIndices.filter(lessOrEqual);
                if (beforeStartIndices.length > 0) {
                    lineStartIndex = beforeStartIndices.pop() || -1;
                    beforeLineCount = beforeStartIndices.length;
                    if (beforeStartIndices.length > self._settings.linesBefore) {
                        beforeStartIndices = beforeStartIndices.slice(
                            beforeStartIndices.length - self._settings.linesBefore);
                    }
                }
                lineEndIndex = endLineIndices[startLineIndices.indexOf(lineStartIndex)];
                const line: string = s.substring(lineStartIndex, lineEndIndex);
                if (self._settings.linesBefore && beforeLineCount) {
                    linesBefore = Finder.getLinesBefore(s, beforeStartIndices,
                        startLineIndices, endLineIndices);
                }
                if (self._settings.linesAfter) {
                    let afterStartIndices: number[] = startLineIndices.filter(greaterThan);
                    if (afterStartIndices.length > self._settings.linesAfter) {
                        afterStartIndices = afterStartIndices.slice(0,
                            self._settings.linesAfter);
                    }
                    linesAfter = Finder.getLinesAfter(s, afterStartIndices,
                        startLineIndices, endLineIndices);
                }
                const matchStartIndex: number = match.index - lineStartIndex + 1;
                const matchEndIndex: number = pattern.lastIndex - lineStartIndex + 1;
                if ((self._settings.linesBefore === 0 || self.linesBeforeMatch(linesBefore)) &&
                    (self._settings.linesAfter === 0 || self.linesAfterMatch(linesAfter))) {
                    const findResult: FindResult = new FindResult(
                        pattern,
                        beforeLineCount+1,
                        matchStartIndex,
                        matchEndIndex,
                        line,
                        linesBefore,
                        linesAfter);
                    results.push(findResult);
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
        return ((inPatterns.length === 0 || Finder.anyMatchesAnyPattern(lines, inPatterns)) &&
               (outPatterns.length === 0 || ! Finder.anyMatchesAnyPattern(lines, outPatterns)));
    }

    private linesBeforeMatch(linesBefore: string[]): boolean {
        return Finder.linesMatch(linesBefore, this._settings.inLinesBeforePatterns,
            this._settings.outLinesBeforePatterns);
    }

    private linesAfterMatch(linesAfter: string[]): boolean {
        return Finder.linesMatch(linesAfter, this._settings.inLinesAfterPatterns,
            this._settings.outLinesAfterPatterns);
    }

    private findTextFileLines(findfile: FindFile): void {
        const self = this;
        const lines: string[] = FileUtil.getFileLines(findfile.relativePath(), this._settings.textFileEncoding);
        const results: FindResult[] = this.findLines(lines);
        results.forEach(function(r: FindResult) {
            r.file = findfile;
            self.addFindResult(r);
        });
    }

    // return results so that filepath can be added to them
    public findLines(lines: string[]): FindResult[] {
        const self = this;
        let linenum = 0;
        let pattern: RegExp;
        const linesBefore: string[] = [];
        const linesAfter: string[] = [];
        const results: FindResult[] = [];
        const patternResults: {[index: string]:number} = {};
        while (true) {
            if (Object.keys(patternResults).length === this._settings.findPatterns.length) {
                break;
            }
            let line = "";
            if (linesAfter.length > 0) {
                line = linesAfter.shift() || '';
            } else if (lines.length > 0) {
                line = lines.shift() || '';
            } else {
                break;
            }
            linenum += 1;
            if (this._settings.linesAfter > 0) {
                while (linesAfter.length < this._settings.linesAfter && lines.length > 0) {
                    linesAfter.push(lines.shift() || '');
                }
            }
            this._settings.findPatterns.forEach(function(p: RegExp) {
                pattern = new RegExp(p.source, "g");
                let match = pattern.exec(line);
                while (match) {
                    if ((self._settings.linesBefore === 0 || self.linesBeforeMatch(linesBefore)) &&
                        (self._settings.linesAfter === 0 || self.linesAfterMatch(linesAfter))) {
                        results.push(new FindResult(
                            pattern,
                            linenum,
                            match.index+1,
                            pattern.lastIndex+1,
                            line,
                            linesBefore,
                            linesAfter));
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

    private addFindResult(result: FindResult): void {
        this.results.push(result);
    }

    private static cmpFindResults(r1: FindResult, r2: FindResult): number {
        let pathCmp = 0;
        if (r1.file && r2.file)
            pathCmp = r1.file.pathname.localeCompare(r2.file.pathname);
        if (pathCmp === 0) {
            let fileCmp = 0;
            if (r1.file && r2.file)
                fileCmp = r1.file.filename.localeCompare(r2.file.filename);
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

    public printFindResults(): void {
        // first sort the results
        this.results.sort(Finder.cmpFindResults);
        const formatter = new FindResultFormatter(this._settings);
        common.log("\nFind results " + `(${this.results.length}):`);
        this.results.forEach(r => common.log(formatter.format(r)));
    }

    public getMatchingDirs(): string[] {
        const dirs: string[] = this.results.filter(r => r.file).map(r => r.file!.pathname);
        return common.setFromArray(dirs);
    }

    public printMatchingDirs(): void {
        const dirs: string[] = this.getMatchingDirs();
        common.log("\nDirectories with matches " + `(${dirs.length}):`);
        dirs.forEach(d => common.log(d));
    }

    public getMatchingFiles(): string[] {
        const files: string[] = this.results.filter(r => r.file).map(r => r.file!.relativePath());
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
