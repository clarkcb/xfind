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
const {FileType} = require("./filetype");
const {FileTypes} = require('./filetypes');
const {FileUtil} = require('./fileutil');
const {FindError} = require('./finderror');
const {SortBy} = require("./sortby");

class Finder {
    settings;
    fileTypes;

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
                    assert.ok(this.filterFile(p, stat),
                        'Startpath does not match find settings');
                } else {
                    assert.ok(false, 'Startpath not findable file type');
                }
            });
            if (this.settings.maxDepth > -1 && this.settings.minDepth > -1) {
                assert.ok(this.settings.maxDepth >= this.settings.minDepth,
                  'Invalid range for mindepth and maxdepth');
            }
            if (this.settings.maxLastMod > 0 && this.settings.minLastMod > 0) {
                assert.ok(this.settings.maxLastMod >= this.settings.minLastMod,
                  'Invalid range for minlastmod and maxlastmod');
            }
            if (this.settings.maxSize > 0 && this.settings.minSize > 0) {
                assert.ok(this.settings.maxSize >= this.settings.minSize,
                  'Invalid range for minsize and maxsize');
            }

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
        if (!this.settings.includeHidden) {
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

    isMatchingFile(file, stat) {
        // if (!this.settings.includeHidden && FileUtil.isHidden(file)) {
        //     return false;
        // }
        let fr = this.filePathToFileResult(file, stat);
        return this.isMatchingFileResult(fr);
    }

    isMatchingFileResult(fr) {
        // if (!this.settings.includeHidden && FileUtil.isHidden(file)) {
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
        if ((this.settings.maxLastMod > 0 && fr.lastMod > this.settings.maxLastMod) ||
          (this.settings.minLastMod > 0 && fr.lastMod < this.settings.minLastMod)) {
            return false;
        }
        if ((this.settings.maxSize > 0 && fr.fileSize > this.settings.maxSize) ||
          (this.settings.minSize > 0 && fr.fileSize < this.settings.minSize)) {
            return false;
        }
        return true;
    }

    isMatchingArchiveFile(file) {
        // if (FileUtil.isHidden(file) && this.settings.excludeHidden) {
        //     return false;
        // }
        if (this.settings.inArchiveExtensions.length || this.settings.outArchiveExtensions.length) {
            let ext = FileUtil.getExtension(file);
            if ((this.settings.inArchiveExtensions.length &&
                    !this.matchesAnyElement(ext, this.settings.inArchiveExtensions)) ||
                (this.settings.outArchiveExtensions.length &&
                    this.matchesAnyElement(ext, this.settings.outArchiveExtensions))) {
                return false;
            }
        }
        if (this.settings.inArchiveFilePatterns.length &&
            !this.matchesAnyPattern(file, this.settings.inArchiveFilePatterns)) {
            return false;
        }
        return !(this.settings.outArchiveFilePatterns.length &&
            this.matchesAnyPattern(file, this.settings.outArchiveFilePatterns));
    }

    filterFile(f, stat) {
        if (!this.settings.includeHidden && FileUtil.isHidden(f)) {
            return false;
        }
        if (this.fileTypes.isArchiveFile(f)) {
            return (this.settings.findArchives && this.isMatchingArchiveFile(f));
        }
        return (!this.settings.archivesOnly && this.isMatchingFile(f, stat));
    }

    filePathToFileResult(fp, stat) {
        const dirname = path.dirname(fp) || '.';
        const fileName = path.basename(fp);
        let fileSize = 0;
        let lastMod = 0;
        if (this.settings.needLastMod() || this.settings.needSize()) {
            stat = stat || fs.statSync(fp);
            fileSize = stat.size;
            lastMod = stat.mtime.getTime();
        }
        return new FileResult(dirname, fileName, this.fileTypes.getFileType(fileName), fileSize, lastMod);
    }

    async filterToFileResult(fp, stat) {
        if (!this.settings.includeHidden && FileUtil.isHidden(fp)) {
            return null;
        }
        const fr = this.filePathToFileResult(fp, stat);
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

    async recGetFileResults(currentDir, depth) {
        if (this.settings.maxDepth > 0 && depth > this.settings.maxDepth) {
            return [];
        }
        let findDirs = [];
        let fileResults = [];
        let files = await fsReaddirAsync(currentDir);
        let filePaths = files.map(f => {
            return path.join(currentDir, f);
        });
        for (let filePath of filePaths) {
            const stats = fs.statSync(filePath);
            if (stats.isDirectory()) {
                if (this.settings.recursive && this.isMatchingDir(filePath)) {
                    findDirs.push(filePath);
                }
            } else if (stats.isFile()) {
                if (depth >= this.settings.minDepth) {
                    const fr = await this.filterToFileResult(filePath, stats);
                    if (fr !== null) {
                        fileResults.push(fr);
                    }
                }
            }
        }

        const subDirFindFileArrays = await Promise.all(findDirs.map(d => this.recGetFileResults(d, depth + 1)));
        subDirFindFileArrays.forEach(subDirFindFiles => {
            fileResults = fileResults.concat(subDirFindFiles);
        });
        return fileResults;
    }

    async getFileResults(startPath) {
        let fileResults = [];
        let stats = await fsStatAsync(startPath);
        if (stats.isDirectory()) {
            // if max_depth is zero, we can skip since a directory cannot be a result
            if (this.settings.maxDepth === 0) {
                return [];
            }
            if (this.isMatchingDir(startPath)) {
                fileResults = await this.recGetFileResults(startPath, 1);
            } else {
                throw new FindError("startPath does not match find criteria");
            }
        } else if (stats.isFile()) {
            // if min_depth > zero, we can skip since the file is at depth zero
            if (this.settings.minDepth > 0) {
                return [];
            }
            const dirname = path.dirname(startPath) || '.';
            if (this.isMatchingDir(dirname)) {
                let fr = this.filterToFileResult(startPath, stats);
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
        if (fr1.fileSize === fr2.fileSize) {
            return this.cmpFileResultsByPath(fr1, fr2);
        }
        return fr1.fileSize - fr2.fileSize;
    }

    cmpFileResultsByType(fr1, fr2) {
        if (fr1.fileType === fr2.fileType) {
            return this.cmpFileResultsByPath(fr1, fr2);
        }
        return fr1.fileType - fr2.fileType;
    }

    cmpFileResultsByLastMod(fr1, fr2) {
        if (fr1.lastMod === fr2.lastMod) {
            return this.cmpFileResultsByPath(fr1, fr2);
        }
        return fr1.lastMod - fr2.lastMod;
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

        const pathFileResultsArrays =
          await Promise.all(this.settings.paths.map(d => this.getFileResults(d)));
        pathFileResultsArrays.forEach(pathFileResults => {
            fileResults = fileResults.concat(pathFileResults);
        });

        return fileResults;
    }
}

exports.Finder = Finder;
