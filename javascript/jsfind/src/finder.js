/*
 * finder.js
 *
 * performs the finding based on the given FindSettings instance
 */

const assert = require('assert');
const fs = require('fs');
const path = require('path');
const { promisify } = require('util');
const fsStatAsync = promisify(fs.stat);
const fsReaddirAsync = promisify(fs.readdir);

const {FileResult} = require('./fileresult');
const {FileTypes} = require('./filetypes');
const {FileUtil} = require('./fileutil');
const {FindError} = require('./finderror');
const {FileType} = require("./filetype");
const {SortBy} = require("./sortby");

class Finder {
    'use strict'

    settings = null;
    fileTypes = null;

    constructor(settings) {
        this.settings = settings;
        this.fileTypes = new FileTypes();
        this.validateSettings();
    }

    validateSettings() {
        try {
            assert.ok(this.settings.paths.length > 0, 'Startpath not defined');
            this.settings.paths.forEach(p => {
                fs.accessSync(p, fs.constants.F_OK | fs.constants.R_OK);

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
            });

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
        return patterns.some((p) => s.search(p) > -1);
    }

    anyMatchesAnyPattern(ss, patterns) {
        return ss.some((s) => this.matchesAnyPattern(s, patterns));
    }

    isMatchingDir(dir) {
        if (FileUtil.isDotDir(dir)) {
            return true;
        }
        if (this.settings.excludeHidden) {
            let nonDotElems = dir.split(path.sep).filter(p => !this.matchesAnyElement(p, ['.','..']));
            if (nonDotElems.length === 0) {
                return true;
            }
            if (nonDotElems.some((p) => FileUtil.isHidden(p))) {
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

    isMatchingFile(file) {
        // if (FileUtil.isHidden(file) && this.settings.excludeHidden) {
        //     return false;
        // }
        if (this.settings.inExtensions.length || this.settings.outExtensions.length) {
            let ext = FileUtil.getExtension(file);
            if ((this.settings.inExtensions.length &&
                    !this.matchesAnyElement(ext, this.settings.inExtensions)) ||
                (this.settings.outExtensions.length &&
                    this.matchesAnyElement(ext, this.settings.outExtensions))) {
                return false;
            }
        }
        if ((this.settings.inFilePatterns.length &&
                !this.matchesAnyPattern(file, this.settings.inFilePatterns)) ||
            (this.settings.outFilePatterns.length &&
                this.matchesAnyPattern(file, this.settings.outFilePatterns))) {
            return false;
        }
        let fileType = this.fileTypes.getFileType(file);
        return !((this.settings.inFileTypes.length &&
            !this.matchesAnyElement(fileType, this.settings.inFileTypes)) ||
            (this.settings.outFileTypes.length &&
                this.matchesAnyElement(fileType, this.settings.outFileTypes)));
    }

    isMatchingFileResult(fr) {
        // if (FileUtil.isHidden(file) && this.settings.excludeHidden) {
        //     return false;
        // }
        if (this.settings.inExtensions.length || this.settings.outExtensions.length) {
            let ext = FileUtil.getExtension(fr.fileName);
            if ((this.settings.inExtensions.length &&
                    !this.matchesAnyElement(ext, this.settings.inExtensions)) ||
                (this.settings.outExtensions.length &&
                    this.matchesAnyElement(ext, this.settings.outExtensions))) {
                return false;
            }
        }
        if ((this.settings.inFilePatterns.length &&
                !this.matchesAnyPattern(fr.fileName, this.settings.inFilePatterns)) ||
            (this.settings.outFilePatterns.length &&
                this.matchesAnyPattern(fr.fileName, this.settings.outFilePatterns))) {
            return false;
        }
        if ((this.settings.inFileTypes.length &&
            !this.matchesAnyElement(fr.fileType, this.settings.inFileTypes)) ||
            (this.settings.outFileTypes.length &&
                this.matchesAnyElement(fr.fileType, this.settings.outFileTypes))) {
            return false;
        }
        if (fr.stat !== null) {
            if ((this.settings.maxLastMod !== null && fr.stat.mtime.getTime() > this.settings.maxLastMod.getTime()) ||
                (this.settings.minLastMod !== null && fr.stat.mtime.getTime() < this.settings.minLastMod.getTime())) {
                return false;
            }
            if ((this.settings.maxSize > 0 && fr.stat.size > this.settings.maxSize) ||
                (this.settings.minSize > 0 && fr.stat.size < this.settings.minSize)) {
                return false;
            }
        }
        return true;
    }

    isMatchingArchiveFile(file) {
        // if (FileUtil.isHidden(file) && this.settings.excludeHidden) {
        //     return false;
        // }
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

    filterFile(f) {
        if (this.settings.excludeHidden && FileUtil.isHidden(f)) {
            return false;
        }
        if (this.fileTypes.isArchiveFile(f)) {
            return (this.settings.findArchives && this.isMatchingArchiveFile(f));
        }
        return (!this.settings.archivesOnly && this.isMatchingFile(f));
    }

    filterToFileResult(fp) {
        if (this.settings.excludeHidden && FileUtil.isHidden(fp)) {
            return null;
        }
        const dirname = path.dirname(fp) || '.';
        const fileName = path.basename(fp);
        let stat = null;
        if (this.settings.needStat()) {
            stat = fs.statSync(fp);
        }
        let fr = new FileResult(dirname, fileName, this.fileTypes.getFileType(fileName), stat);
        if (fr.fileType === FileType.ARCHIVE) {
            if (this.settings.findArchives && this.isMatchingArchiveFile(fr.fileName)) {
                return fr;
            }
            return null;
        }
        if (!this.settings.archivesOnly && this.isMatchingFileResult(fr)) {
            return fr;
        }
        return null;
    }

    async recGetFileResults(currentDir) {
        let findDirs = [];
        let fileResults = [];
        let files = await fsReaddirAsync(currentDir);
        files.map(f => {
            return path.join(currentDir, f);
        }).forEach(f => {
            let stats = fs.statSync(f);
            if (stats.isDirectory() && this.settings.recursive && this.isMatchingDir(f)) {
                findDirs.push(f);
            } else if (stats.isFile()) {
                // const dirname = path.dirname(f) || '.';
                // const fileName = path.basename(f);
                // if (this.filterFile(fileName)) {
                //     const fileType = this.fileTypes.getFileType(fileName);
                //     const sf = new FileResult(dirname, fileName, fileType);
                //     fileResults.push(sf);
                // }
                let fr = this.filterToFileResult(f);
                if (fr !== null) {
                    fileResults.push(fr);
                }
            }
        });

        const subDirFindFileArrays = await Promise.all(findDirs.map(d => this.recGetFileResults(d)));
        subDirFindFileArrays.forEach(subDirFindFiles => {
            fileResults = fileResults.concat(subDirFindFiles);
        });
        return fileResults;
    }

    async getFileResults(startPath) {
        let fileResults = [];
        let stats = await fsStatAsync(startPath);
        if (stats.isDirectory()) {
            if (this.isMatchingDir(startPath)) {
                fileResults = await this.recGetFileResults(startPath);
            } else {
                throw new FindError("startPath does not match find criteria");
            }
        } else if (stats.isFile()) {
            const dirname = path.dirname(startPath) || '.';
            if (this.isMatchingDir(dirname)) {
                let fr = this.filterToFileResult(startPath);
                if (fr !== null) {
                    fileResults.push(fr);
                } else {
                    throw new FindError("startPath does not match find criteria");
                }
            } else {
                throw new FindError("startPath does not match find criteria");
            }
        }
        this.sortFileResults(fileResults);
        return fileResults;
    }

    cmpFileResultsByPath(fr1, fr2) {
        const [path1, path2] = this.settings.sortCaseInsensitive ?
            [fr1.path.toLowerCase(), fr2.path.toLowerCase()] :
            [fr1.path, fr2.path];
        if (path1 === path2) {
            const [fileName1, fileName2] = this.settings.sortCaseInsensitive ?
                [fr1.fileName.toLowerCase(), fr2.fileName.toLowerCase()] :
                [fr1.fileName, fr2.fileName];
            return fileName1 < fileName2 ? -1 : 1;
        }
        return path1 < path2 ? -1 : 1;
    }

    cmpFileResultsByName(fr1, fr2) {
        const [fileName1, fileName2] = this.settings.sortCaseInsensitive ?
            [fr1.fileName.toLowerCase(), fr2.fileName.toLowerCase()] :
            [fr1.fileName, fr2.fileName];
        if (fileName1 === fileName2) {
            const [path1, path2] = this.settings.sortCaseInsensitive ?
                [fr1.path.toLowerCase(), fr2.path.toLowerCase()] :
                [fr1.path, fr2.path];
            return path1 < path2 ? -1 : 1;
        }
        return fileName1 < fileName2 ? -1 : 1;
    }

    cmpFileResultsBySize(fr1, fr2) {
        if (fr1.stat !== null && fr2.stat !== null) {
            if (fr1.stat.size === fr2.stat.size) {
                return this.cmpFileResultsByPath(fr1, fr2);
            }
            return fr1.stat.size - fr2.stat.size;
        }
        return 0;
    }

    cmpFileResultsByType(fr1, fr2) {
        if (fr1.fileType === fr2.fileType) {
            return this.cmpFileResultsByPath(fr1, fr2);
        }
        return fr1.fileType - fr2.fileType;
    }

    cmpFileResultsByLastMod(fr1, fr2) {
        if (fr1.stat !== null && fr2.stat !== null) {
            if (fr1.stat.mtime.getTime() === fr2.stat.mtime.getTime()) {
                return this.cmpFileResultsByPath(fr1, fr2);
            }
            return fr1.stat.mtime.getTime() - fr2.stat.mtime.getTime();
        }
        return 0;
    }

    sortFileResults(fileResults) {
        if (this.settings.sortBy === SortBy.FILENAME) {
            fileResults.sort((a, b) => this.cmpFileResultsByName(a, b));
        } else if (this.settings.sortBy === SortBy.FILESIZE) {
            fileResults.sort((a, b) => this.cmpFileResultsBySize(a, b));
        } else if (this.settings.sortBy === SortBy.FILETYPE) {
            fileResults.sort((a, b) => this.cmpFileResultsByType(a, b));
        } else if (this.settings.sortBy === SortBy.LASTMOD) {
            fileResults.sort((a, b) => this.cmpFileResultsByLastMod(a, b));
        } else {
            fileResults.sort((a, b) => this.cmpFileResultsByPath(a, b));
        }
        if (this.settings.sortDescending) {
            fileResults.reverse();
        }
    }

    async find() {
        // get the file results
        let fileResults = [];

        const pathFileResultsArrays = await Promise.all(this.settings.paths.map(d => this.getFileResults(d)));
        pathFileResultsArrays.forEach(pathFileResults => {
            fileResults = fileResults.concat(pathFileResults);
        });

        return fileResults;
    }
}

exports.Finder = Finder;
