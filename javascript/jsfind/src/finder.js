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
const FileUtil = require('./fileutil');
const {FindError} = require('./finderror');
const {FileType} = require("./filetype");

class Finder {
    'use strict'

    constructor(settings) {
        this.settings = settings;
        this.filetypes = new FileTypes();
        this.validateSettings();
    }

    validateSettings() {
        try {
            assert.ok(this.settings.paths.length > 0, 'Startpath not defined');
            this.settings.paths.forEach(p => {
                fs.accessSync(p, fs.constants.F_OK | fs.constants.R_OK);

                const stat = fs.lstatSync(p);
                if (stat.isDirectory()) {
                    assert.ok(this.isFindDir(p),
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

    isFindDir(dir) {
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

    isFindFile(file) {
        // if (FileUtil.isHidden(file) && this.settings.excludeHidden) {
        //     return false;
        // }
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
        if (this.filetypes.isArchiveFile(f)) {
            return (this.settings.findArchives && this.isArchiveFindFile(f));
        }
        return (!this.settings.archivesOnly && this.isFindFile(f));
    }

    filterToFileResult(f) {
        if (this.settings.excludeHidden && FileUtil.isHidden(f)) {
            return null;
        }
        const dirname = path.dirname(f) || '.';
        const filename = path.basename(f);
        let fr = new FileResult(dirname, filename, this.filetypes.getFileType(filename));
        if (fr.filetype === FileType.ARCHIVE) {
            if (this.settings.findArchives && this.isArchiveFindFile(fr.filename)) {
                return fr;
            }
            return null;
        }
        if (!this.settings.archivesOnly && this.isFindFile(fr.filename)) {
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
            if (stats.isDirectory() && this.settings.recursive && this.isFindDir(f)) {
                findDirs.push(f);
            } else if (stats.isFile()) {
                // const dirname = path.dirname(f) || '.';
                // const filename = path.basename(f);
                // if (this.filterFile(filename)) {
                //     const filetype = this.filetypes.getFileType(filename);
                //     const sf = new FileResult(dirname, filename, filetype);
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
            if (this.isFindDir(startPath)) {
                fileResults = await this.recGetFileResults(startPath);
            } else {
                throw new FindError("startPath does not match find criteria");
            }
        } else if (stats.isFile()) {
            const dirname = path.dirname(startPath) || '.';
            if (this.isFindDir(dirname) && this.filterFile(startPath)) {
                const filename = path.basename(startPath);
                const filetype = this.filetypes.getFileType(filename);
                const sf = new FileResult(dirname, filename, filetype);
                fileResults.push(sf);
            } else {
                throw new FindError("startPath does not match find criteria");
            }
        }
        return fileResults;
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
