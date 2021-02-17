/*
 * finder.js
 *
 * performs the finding based on the given FindSettings instance
 */

const assert = require('assert');
const fs = require('fs');
const fsp = fs.promises;
const path = require('path');
const { promisify } = require('util');
const fsStatAsync = promisify(fs.stat);
const fsReaddirAsync = promisify(fs.readdir);

const common = require('./common');
const {FileType} = require('./filetype');
const {FileTypes} = require('./filetypes');
const FileUtil = require('./fileutil');
const {FindError} = require('./finderror');
const {FindFile} = require('./findfile');
const {FindResult} = require('./findresult');

class Finder {
    'use strict'

    constructor(settings) {
        this.settings = settings;
        this.binaryEncoding = 'latin1';
        // from https://github.com/nodejs/node/blob/master/lib/buffer.js
        this.supportedEncodings = ['utf-8', 'utf8', 'latin1', 'ascii', 'ucs2',  'ucs-2', 'utf16le',
            'binary', 'base64', 'hex'];
        this.filetypes = new FileTypes();
        this.results = [];
        this.validateSettings();
    }

    validateSettings() {
        try {
            assert.ok(!!this.settings.startPath, 'Startpath not defined');

            fs.accessSync(this.settings.startPath, fs.constants.F_OK | fs.constants.R_OK);

            const stat = fs.lstatSync(this.settings.startPath);

            if (stat.isDirectory()) {
                assert.ok(this.isFindDir(this.settings.startPath),
                    'Startpath does not match find settings');
            } else if (stat.isFile()) {
                assert.ok(this.filterFile(this.settings.startPath),
                    'Startpath does not match find settings');
            } else {
                assert.ok(false, 'Startpath not findable file type');
            }
            assert.ok(this.settings.findPatterns.length, 'No find patterns defined');
            assert.ok(this.supportedEncodings.indexOf(this.settings.textFileEncoding) > -1,
                'Invalid encoding');
            assert.ok(this.settings.linesBefore > -1, 'Invalid linesbefore');
            assert.ok(this.settings.linesAfter > -1, 'Invalid linesafter');
            assert.ok(this.settings.maxLineLength > -1, 'Invalid maxlinelength');

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

    matchesAnyElement(s, elements) {
        return elements.indexOf(s) > -1;
    }

    matchesAnyPattern(s, patterns) {
        return patterns.some((p, i, arr) => s.find(p) > -1);
    }

    anyMatchesAnyPattern(ss, patterns) {
        return ss.some((s, i, arr) => this.matchesAnyPattern(s, patterns));
    }

    isFindDir(dir) {
        if (FileUtil.isDotDir(dir)) {
            return true;
        }
        if (this.settings.excludeHidden) {
            let nonDotElems = dir.split(path.sep).filter(p => !this.matchesAnyElement(p, ['.','..']));
            if (nonDotElems.length === 0) {
                return true;
            }
            if (nonDotElems.some((p, i, arr) => FileUtil.isHidden(p))) {
                return false;
            }
        }
        if (this.settings.inDirPatterns.length && !this.matchesAnyPattern(dir,
            this.settings.inDirPatterns)) {
            return false;
        }
        return !(this.settings.outDirPatterns.length && this.matchesAnyPattern(dir,
            this.settings.outDirPatterns));
    }

    isFindFile(file) {
        if (FileUtil.isHidden(file) && this.settings.excludeHidden) {
            return false;
        }
        let ext = FileUtil.getExtension(file);
        if ((this.settings.inExtensions.length &&
            !this.matchesAnyElement(ext, this.settings.inExtensions))
            || (this.settings.outExtensions.length &&
                this.matchesAnyElement(ext, this.settings.outExtensions))
            || (this.settings.inFilePatterns.length &&
                !this.matchesAnyPattern(file, this.settings.inFilePatterns))
            || (this.settings.outFilePatterns.length &&
                this.matchesAnyPattern(file, this.settings.outFilePatterns))) {
            return false;
        }
        let filetype = this.filetypes.getFileType(file);
        return !((this.settings.inFileTypes.length &&
            !this.matchesAnyElement(filetype, this.settings.inFileTypes))
            || (this.settings.outFileTypes.length &&
                this.matchesAnyElement(filetype, this.settings.outFileTypes)));
    }

    isArchiveFindFile(file) {
        if (FileUtil.isHidden(file) && this.settings.excludeHidden) {
            return false;
        }
        let ext = FileUtil.getExtension(file);
        if (this.settings.inArchiveExtensions.length &&
            !this.matchesAnyElement(ext, this.settings.inArchiveExtensions)) {
            return false;
        }
        if (this.settings.outArchiveExtensions.length &&
            this.matchesAnyElement(ext, this.settings.outArchiveExtensions)) {
            return false;
        }
        if (this.settings.inArchiveFilePatterns.length &&
            !this.matchesAnyPattern(file, this.settings.inArchiveFilePatterns)) {
            return false;
        }
        return !(this.settings.outArchiveFilePatterns.length &&
            this.matchesAnyPattern(file, this.settings.outArchiveFilePatterns));
    }

    async getFindFiles(startPath) {
        let findFiles = [];
        try {
            let stats = await fsStatAsync(startPath);
            if (stats.isDirectory()) {
                if (this.isFindDir(startPath)) {
                    findFiles = await this.recGetFindFiles(startPath);
                } else {
                    throw new FindError("startPath does not match find criteria");
                }
            } else if (stats.isFile()) {
                const dirname = path.dirname(startPath) || '.';
                if (this.isFindDir(dirname) && this.filterFile(startPath)) {
                    const filename = path.basename(startPath);
                    const filetype = this.filetypes.getFileType(filename);
                    const sf = new FindFile(dirname, filename, filetype);
                    findFiles.push(sf);
                } else {
                    throw new FindError("startPath does not match find criteria");
                }
            }
            return findFiles;

        } catch (err) {
            // common.log(err);
            throw err;
        }
    }

    async recGetFindFiles(currentDir) {
        let findDirs = [];
        let findFiles = [];
        let files = await fsReaddirAsync(currentDir);
        files.map(f => {
            return path.join(currentDir, f);
        }).forEach(f => {
            let stats = fs.statSync(f);
            if (stats.isDirectory() && this.settings.recursive && this.isFindDir(f)) {
                findDirs.push(f);
            } else if (stats.isFile() && this.filterFile(f)) {
                const dirname = path.dirname(f) || '.';
                const filename = path.basename(f);
                const filetype = this.filetypes.getFileType(filename);
                const sf = new FindFile(dirname, filename, filetype);
                findFiles.push(sf);
            }
        });

        const subDirFindFileArrays = await Promise.all(findDirs.map(d => this.recGetFindFiles(d)));
        subDirFindFileArrays.forEach(subDirFindFiles => {
            findFiles = findFiles.concat(subDirFindFiles);
        });
        return findFiles;
    }

    filterFile(f) {
        if (this.filetypes.isArchiveFile(f)) {
            return (this.settings.findArchives && this.isArchiveFindFile(f));
        }
        return (!this.settings.archivesOnly && this.isFindFile(f));
    }

    async find() {
        try {
            // get the find files
            let findfiles = await this.getFindFiles(this.settings.startPath);

            if (this.settings.verbose) {
                let dirs = findfiles.map(sf => sf.pathname);
                dirs = common.setFromArray(dirs);
                dirs.sort();
                common.log("\nDirectories to be found " + `(${dirs.length}):`);
                dirs.forEach(d => common.log(d));

                common.log("\nFiles to be found " + `(${findfiles.length}):`);
                findfiles.forEach(sf => common.log(sf.relativePath()));
                common.log("");
            }

            // find the files
            let results = [];
            const findFileResultsArrays = await Promise.all(findfiles.map(sf => this.findFile(sf)));
            findFileResultsArrays.forEach(findFileResults => {
                results = results.concat(findFileResults);
            });

            if (this.settings.verbose) {
                common.log('Find complete.');
            }

            return results;

        } catch (err) {
            throw err;
        }
    }

    async findFile(findfile) {
        let results = [];
        switch (findfile.filetype) {
            case FileType.CODE:
            case FileType.TEXT:
            case FileType.XML:
                results = await this.findTextFile(findfile);
                break;
            case FileType.BINARY:
                results = await this.findBinaryFile(findfile);
                break;
            default:
                // TODO: add message about unsupported filetype
                break;
        }
        return results;
    }

    async findBinaryFile(findfile) {
        if (this.settings.verbose) {
            common.log(`Finding binary file: "${findfile}"`);
        }

        const contents = await FileUtil.getFileContentsAsync(findfile.relativePath(), this.binaryEncoding);
        let results = [];

        const findPattern = pattern => {
            pattern = new RegExp(pattern.source, 'g');
            let patternResults = [];
            let match = pattern.exec(contents);
            while (match) {
                patternResults.push(new FindResult(
                    pattern,
                    findfile,
                    0,
                    match.index+1,
                    pattern.lastIndex+1,
                    null,
                    [],
                    []));
                if (this.settings.firstMatch) {
                    return patternResults;
                }
                match = pattern.exec(contents);
            }
            return patternResults;
        }

        const patternResultArrays = await Promise.all(this.settings.findPatterns.map(p => findPattern(p)));
        patternResultArrays.forEach(patternResults => {
            results = results.concat(patternResults);
        });
        return results;
    }

    async findTextFile(findfile) {
        if (this.settings.verbose) {
            common.log(`Finding text file ${findfile}`);
        }
        let results;
        if (this.settings.multilineFind) {
            results = await this.findTextFileContents(findfile);
        } else {
            results = await this.findTextFileLines(findfile);
        }
        return results;
    }

    async findTextFileContents(findfile) {
        const contents = await FileUtil.getFileContentsAsync(findfile.relativePath(), this.settings.textFileEncoding);
        let stringResults = await this.findMultiLineString(contents);
        return stringResults.map(r => {
            return new FindResult(r.pattern, findfile, r.linenum, r.matchStartIndex, r.matchEndIndex, r.line,
                r.linesBefore, r.linesAfter, this.settings.maxLineLength, this.settings.colorize);
        });
    }

    getNewLineIndices(s) {
        let indices = [];
        for (let i = 0; i < s.length; i++) {
            if (s.charAt(i) === "\n") {
                indices.push(i);
            }
        }
        return indices;
    }

    getLinesAtIndices(s, atIndices, startLineIndices, endLineIndices) {
        if (atIndices.length === 0)
            return [];
        let lines = [];
        atIndices.forEach(i => {
            let line = s.substring(i, endLineIndices[startLineIndices.indexOf(i)]);
            lines.push(line);
        });
        return lines;
    }

    getLinesBefore(s, beforeStartIndices, startLineIndices, endLineIndices) {
        return this.getLinesAtIndices(s, beforeStartIndices, startLineIndices, endLineIndices);
    }

    getLinesAfter(s, afterStartIndices, startLineIndices, endLineIndices) {
        return this.getLinesAtIndices(s, afterStartIndices, startLineIndices, endLineIndices);
    }

    getLessThanOrEqual(matchVal) {
        return i => { return i <= matchVal; };
    }

    getGreaterThan(matchVal) {
        return i => { return i > matchVal; };
    }

    plusOne(i) {
        return i + 1;
    }

    async findMultiLineString(s) {
        let linesBefore = [];
        let linesAfter = [];
        let results = [];
        try {
            let newLineIndices = this.getNewLineIndices(s);
            let startLineIndices = [0].concat(newLineIndices.map(this.plusOne));
            let endLineIndices = newLineIndices.concat([s.length - 1]);

            const findPattern = pattern => {
                pattern = new RegExp(pattern.source, 'g');
                let patternResults = [];
                let match = pattern.exec(s);
                while (match) {
                    if (this.settings.firstMatch && patternResults.length > 0) {
                        return patternResults;
                    }
                    let lessOrEqual = this.getLessThanOrEqual(match.index);
                    let greaterThan = this.getGreaterThan(match.index);
                    let lineStartIndex = 0;
                    let lineEndIndex = s.length - 1;
                    let beforeLineCount = 0;
                    let beforeStartIndices = startLineIndices.filter(lessOrEqual);
                    if (beforeStartIndices.length > 0) {
                        lineStartIndex = beforeStartIndices.pop();
                        beforeLineCount = beforeStartIndices.length;
                        if (beforeStartIndices.length > this.settings.linesBefore) {
                            beforeStartIndices = beforeStartIndices.slice(
                                beforeStartIndices.length - this.settings.linesBefore);
                        }
                    }
                    lineEndIndex = endLineIndices[startLineIndices.indexOf(lineStartIndex)];
                    let line = s.substring(lineStartIndex, lineEndIndex);
                    if (this.settings.linesBefore && beforeLineCount) {
                        linesBefore = this.getLinesBefore(s, beforeStartIndices,
                            startLineIndices, endLineIndices);
                    }
                    if (this.settings.linesAfter) {
                        let afterStartIndices = startLineIndices.filter(greaterThan);
                        if (afterStartIndices.length > this.settings.linesAfter) {
                            afterStartIndices = afterStartIndices.slice(0,
                                this.settings.linesAfter);
                        }
                        linesAfter = this.getLinesAfter(s, afterStartIndices,
                            startLineIndices, endLineIndices);
                    }
                    let matchStartIndex = match.index - lineStartIndex + 1;
                    let matchEndIndex = pattern.lastIndex - lineStartIndex + 1;
                    if ((this.settings.linesBefore === 0 || this.linesBeforeMatch(linesBefore)) &&
                        (this.settings.linesAfter === 0 || this.linesAfterMatch(linesAfter))) {
                        patternResults.push(new FindResult(
                            pattern,
                            '',
                            beforeLineCount + 1,
                            matchStartIndex,
                            matchEndIndex,
                            line,
                            [].concat(linesBefore),
                            [].concat(linesAfter),
                            this.settings.maxLineLength,
                            this.settings.colorize));
                        if (!(pattern.source in patternResults)) {
                            patternResults[pattern.source] = 1;
                        }
                    }
                    match = pattern.exec(s);
                }
                return patternResults;
            }

            const patternResultArrays = await Promise.all(this.settings.findPatterns.map(p => findPattern(p)));
            patternResultArrays.forEach(patternResults => {
                results = results.concat(patternResults);
            });
            return results;

        } catch (err) {
            throw err;
        }
    }

    linesMatch(lines, inPatterns, outPatterns) {
        return ((inPatterns.length === 0 || this.anyMatchesAnyPattern(lines, inPatterns)) &&
            (outPatterns.length === 0 || ! this.anyMatchesAnyPattern(lines, outPatterns)));
    }

    linesBeforeMatch(linesBefore) {
        return this.linesMatch(linesBefore, this.settings.inLinesBeforePatterns,
            this.settings.outLinesBeforePatterns);
    }

    linesAfterMatch(linesAfter) {
        return this.linesMatch(linesAfter, this.settings.inLinesAfterPatterns,
            this.settings.outLinesAfterPatterns);
    }

    async findTextFileLines(findfile) {
        let lines = FileUtil.getFileLines(findfile.relativePath(), this.settings.textFileEncoding);
        let linesResults = await this.findLines(lines);
        return linesResults.map(r => {
            return new FindResult(r.pattern, findfile, r.linenum, r.matchStartIndex, r.matchEndIndex, r.line,
                r.linesBefore, r.linesAfter, this.settings.maxLineLength, this.settings.colorize);
        });
    }

    // return results so that filepath can be added to them
    async findLines(lines) {
        let linenum = 0;
        let pattern;
        let linesBefore = [];
        let linesAfter = [];
        let results = [];
        let patternResults = {};
        while (true) {
            if (Object.keys(patternResults).length === this.settings.findPatterns.length) {
                break;
            }
            let line = "";
            if (linesAfter.length > 0) {
                line = linesAfter.shift();
            } else if (lines.length > 0) {
                line = lines.shift();
            } else {
                break;
            }
            linenum += 1;
            if (this.settings.linesAfter > 0) {
                while (linesAfter.length < this.settings.linesAfter && lines.length > 0) {
                    linesAfter.push(lines.shift());
                }
            }
            this.settings.findPatterns.forEach(p => {
                pattern = new RegExp(p.source, "g");
                let match = pattern.exec(line);
                while (match) {
                    if ((this.settings.linesBefore === 0 || this.linesBeforeMatch(linesBefore)) &&
                        (this.settings.linesAfter === 0 || this.linesAfterMatch(linesAfter))) {
                        results.push(new FindResult(
                            pattern,
                            '',
                            linenum,
                            match.index+1,
                            pattern.lastIndex+1,
                            line,
                            [].concat(linesBefore),
                            [].concat(linesAfter),
                            this.settings.maxLineLength,
                            this.settings.colorize));
                        if (this.settings.firstMatch) {
                            patternResults[pattern.source] = 1;
                            break;
                        }
                    }
                    match = pattern.exec(line);
                }
            });
            if (this.settings.linesBefore > 0) {
                if (linesBefore.length === this.settings.linesBefore)
                    linesBefore.shift();
                if (linesBefore.length < this.settings.linesBefore)
                    linesBefore.push(line);
            }
        }
        return results;
    }
}

exports.Finder = Finder;
