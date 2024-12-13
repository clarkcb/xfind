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

const {FileResult} = require('./fileresult');
const {FileType} = require("./filetype");
const {FileTypes} = require('./filetypes');
const {FileUtil} = require('./fileutil');
const {FindError} = require('./finderror');
const {SortBy} = require('./sortby');

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
                // Validate existence, accessibility and "findability" of file path (directory or regular file)
                try {
                    fs.accessSync(p, fs.constants.F_OK | fs.constants.R_OK);
                } catch (err) {
                    p = FileUtil.expandPath(p);
                    fs.accessSync(p, fs.constants.F_OK | fs.constants.R_OK);
                }
                const stat = fs.lstatSync(p);
                if (!stat.isDirectory() && !stat.isFile()) {
                    assert.ok(false, 'Startpath is unsupported file type');
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

    isMatchingExtension(ext, inExtensions, outExtensions) {
        return ((inExtensions.length === 0 || this.matchesAnyElement(ext, inExtensions)) &&
          (outExtensions.length === 0 || !this.matchesAnyElement(ext, outExtensions)));
    }

    hasMatchingArchiveExtension(fr) {
        if (this.settings.inArchiveExtensions.length || this.settings.outArchiveExtensions.length) {
            const ext = FileUtil.getExtension(fr.fileName);
            return this.isMatchingExtension(ext, this.settings.inArchiveExtensions, this.settings.outArchiveExtensions);
        }
        return true;
    }

    hasMatchingExtension(fr) {
        if (this.settings.inExtensions.length || this.settings.outExtensions.length) {
            let ext = FileUtil.getExtension(fr.fileName);
            return this.isMatchingExtension(ext, this.settings.inExtensions, this.settings.outExtensions);
        }
        return true;
    }

    isMatchingArchiveFileName(fileName) {
        return ((this.settings.inArchiveFilePatterns.length === 0 ||
            this.matchesAnyPattern(fileName, this.settings.inArchiveFilePatterns)) &&
            (this.settings.outArchiveFilePatterns.length === 0 ||
            !this.matchesAnyPattern(fileName, this.settings.outArchiveFilePatterns)));
    }

    isMatchingFileName(fileName) {
        return ((this.settings.inFilePatterns.length === 0 ||
            this.matchesAnyPattern(fileName, this.settings.inFilePatterns)) &&
            (this.settings.outFilePatterns.length === 0 ||
            !this.matchesAnyPattern(fileName, this.settings.outFilePatterns)));
    }

    isMatchingFileType(fileType) {
        return ((this.settings.inFileTypes.length === 0 ||
            this.matchesAnyElement(fileType, this.settings.inFileTypes)) &&
            (this.settings.outFileTypes.length === 0 ||
            !this.matchesAnyElement(fileType, this.settings.outFileTypes)));
    }

    isMatchingFileSize(fileSize) {
        return ((this.settings.maxSize === 0 || fileSize <= this.settings.maxSize) &&
            (this.settings.minSize === 0 || fileSize >= this.settings.minSize));
    }

    isMatchingLastMod(lastMod) {
        return ((this.settings.maxLastMod === 0 || lastMod <= this.settings.maxLastMod) &&
            (this.settings.minLastMod === 0 || lastMod >= this.settings.minLastMod));
    }

    isMatchingArchiveFileResult(fr) {
        return this.hasMatchingArchiveExtension(fr) &&
          this.isMatchingArchiveFileName(fr.fileName);
    }

    isMatchingFileResult(fr) {
        return this.hasMatchingExtension(fr) &&
            this.isMatchingFileName(fr.fileName) &&
            this.isMatchingFileType(fr.fileType) &&
            this.isMatchingFileSize(fr.fileSize) &&
            this.isMatchingLastMod(fr.lastMod);
    }

    filterToFileResult(fp, stat) {
        if (!this.settings.includeHidden && FileUtil.isHidden(fp)) {
            return null;
        }
        const dirname = path.dirname(fp) || '.';
        const fileName = path.basename(fp);
        const fileType = this.fileTypes.getFileType(fileName);
        if (fileType === FileType.ARCHIVE && !this.settings.includeArchives && !this.settings.archivesOnly) {
            return null;
        }
        let fileSize = 0;
        let lastMod = 0;
        if (this.settings.needLastMod() || this.settings.needSize()) {
            stat = stat || fs.statSync(fp);
            if (this.settings.needSize()) fileSize = stat.size;
            if (this.settings.needLastMod()) lastMod = stat.mtime.getTime();
        }
        const fr = new FileResult(dirname, fileName, fileType, fileSize, lastMod);
        if (fr.fileType === FileType.ARCHIVE) {
            if (this.isMatchingArchiveFileResult(fr)) {
                return fr;
            }
            return null;
        }
        if (!this.settings.archivesOnly && this.isMatchingFileResult(fr)) {
            return fr;
        }
        return null;
    }

    async recGetFileResults(currentDir, minDepth, maxDepth, currentDepth) {
        let fileResults = [];
        let recurse = true;
        if (currentDepth === maxDepth) {
            recurse = false;
        } else if (maxDepth > -1 && currentDepth > maxDepth) {
            return [];
        }
        let findDirs = [];
        let filePaths = (fs.readdirSync(currentDir, {recursive: false}))
          .map(f => path.join(currentDir, f));
        for (let filePath of filePaths) {
            let stats = fs.lstatSync(filePath);
            if (stats.isSymbolicLink() && !this.settings.followSymlinks) {
                continue;
            }
            stats = fs.statSync(filePath);
            if (stats.isDirectory() && recurse && this.isMatchingDir(filePath)) {
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

    async getFileResults(filePath) {
        try {
            fs.accessSync(filePath, fs.constants.F_OK | fs.constants.R_OK);
        } catch (err) {
            filePath = FileUtil.expandPath(filePath);
        }
        const stats = await fsStatAsync(filePath);
        if (stats.isDirectory()) {
            // if max_depth is zero, we can skip since a directory cannot be a result
            if (this.settings.maxDepth === 0) {
                return [];
            }
            if (this.isMatchingDir(filePath)) {
                let maxDepth = this.settings.maxDepth;
                if (!this.settings.recursive) {
                    maxDepth = 1;
                }
                return await this.recGetFileResults(filePath, this.settings.minDepth, maxDepth, 1);
            } else {
                throw new FindError("Startpath does not match find settings");
            }
        } else if (stats.isFile()) {
            // if min_depth > zero, we can skip since the file is at depth zero
            if (this.settings.minDepth > 0) {
                return [];
            }
            const dirname = path.dirname(filePath) || '.';
            if (this.isMatchingDir(dirname)) {
                const fr = this.filterToFileResult(filePath, stats);
                if (fr !== null) {
                    return [fr];
                } else {
                    throw new FindError("Startpath does not match find settings");
                }
            } else {
                throw new FindError("Startpath does not match find settings");
            }
        }
        return [];
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

        this.sortFileResults(fileResults);
        return fileResults;
    }
}

exports.Finder = Finder;
