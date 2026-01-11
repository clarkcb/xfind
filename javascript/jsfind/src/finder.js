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
const {FileResultSorter} = require('./fileresultsorter');
const {FileType} = require("./filetype");
const {FileTypes} = require('./filetypes');
const {FileUtil, ENOENT, EACCES} = require('./fileutil');
const {FindError} = require('./finderror');
const common = require('./common');

const startPathNotDefined = 'Startpath not defined';
const invalidRangeForMinDepthAndMaxDepth = 'Invalid range for mindepth and maxdepth';
const invalidRangeForMinLastModAndMaxLastMod = 'Invalid range for minlastmod and maxlastmod';
const invalidRangeForMinSizeAndMaxSize = 'Invalid range for minsize and maxsize';
const startPathNotFound = 'Startpath not found';
const startPathNotReadable = 'Startpath not readable';
const startPathDoesNotMatchFindSettings = 'Startpath does not match find settings';


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
            assert.ok(this.settings.paths.length > 0, startPathNotDefined);
            this.settings.paths.forEach(p => {
                // Validate existence, accessibility and "findability" of file path (directory or regular file)
                try {
                    fs.accessSync(p, fs.constants.F_OK | fs.constants.R_OK);
                } catch (err) {
                    p = FileUtil.expandPath(p);
                    fs.accessSync(p, fs.constants.F_OK | fs.constants.R_OK);
                }
            });
            if (this.settings.maxDepth > -1 && this.settings.minDepth > -1) {
                assert.ok(this.settings.maxDepth >= this.settings.minDepth,
                  invalidRangeForMinDepthAndMaxDepth);
            }
            if (this.settings.maxLastMod > 0 && this.settings.minLastMod > 0) {
                assert.ok(this.settings.maxLastMod >= this.settings.minLastMod,
                  invalidRangeForMinLastModAndMaxLastMod);
            }
            if (this.settings.maxSize > 0 && this.settings.minSize > 0) {
                assert.ok(this.settings.maxSize >= this.settings.minSize,
                  invalidRangeForMinSizeAndMaxSize);
            }

        } catch (err) {
            let msg = err.message;
            if (err.code === ENOENT) {
                msg = startPathNotFound;
            } else if (err.code === EACCES) {
                msg = startPathNotReadable;
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

    filterDirByHidden(dir) {
        if (!this.settings.includeHidden) {
            return !FileUtil.isHiddenPath(dir);
        }
        return true;
    }

    filterDirByInPatterns(dir) {
        return this.settings.inDirPatterns.length === 0
            || this.matchesAnyPattern(dir, this.settings.inDirPatterns);
    }

    filterDirByOutPatterns(dir) {
        return this.settings.outDirPatterns.length === 0
            || !this.matchesAnyPattern(dir, this.settings.outDirPatterns);
    }

    isMatchingDir(dir) {
        return this.filterDirByHidden(dir)
            && this.filterDirByInPatterns(dir)
            && this.filterDirByOutPatterns(dir);
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

    filterToFileResult(filePath, stat) {
        const dirname = path.dirname(filePath) || '.';
        if (!this.isMatchingDir(dirname)) {
            return null;
        }
        const fileName = path.basename(filePath);
        if (!this.settings.includeHidden && FileUtil.isHiddenName(fileName)) {
            return null;
        }
        const fileType = this.fileTypes.getFileType(fileName);
        if (fileType === FileType.ARCHIVE && !this.settings.includeArchives && !this.settings.archivesOnly) {
            return null;
        }
        let fileSize = 0;
        let lastMod = 0;
        if (this.settings.needLastMod() || this.settings.needSize()) {
            stat = stat || fs.statSync(filePath);
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
            if (stats.isDirectory() && recurse && this.filterDirByHidden(filePath) && this.filterDirByOutPatterns(filePath)) {
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
            if (this.filterDirByHidden(filePath) && this.filterDirByOutPatterns(filePath)) {
                let maxDepth = this.settings.maxDepth;
                if (!this.settings.recursive) {
                    maxDepth = 1;
                }
                return await this.recGetFileResults(filePath, this.settings.minDepth, maxDepth, 1);
            } else {
                throw new FindError(startPathDoesNotMatchFindSettings);
            }
        } else {
            // if min_depth > zero, we can skip since the file is at depth zero
            if (this.settings.minDepth > 0) {
                return [];
            }
            const fr = this.filterToFileResult(filePath, stats);
            if (fr !== null) {
                return [fr];
            } else {
                throw new FindError(startPathDoesNotMatchFindSettings);
            }
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

        const fileResultSorter = new FileResultSorter(this.settings);
        fileResultSorter.sort(fileResults);
        return fileResults;
    }

    getMatchingDirs(fileResults) {
        const dirs = fileResults.map(f => f.path);
        return common.setFromArray(dirs);
    }

    printMatchingDirs(fileResults, formatter) {
        const dirs = this.getMatchingDirs(fileResults);
        if (dirs.length > 0) {
            common.log(`\nMatching directories (${dirs.length}):`);
            dirs.forEach(d => common.log(formatter.formatDirPath(d)));
        } else {
            common.log('\nMatching directories: 0');
        }
    }

    printMatchingFiles(fileResults, formatter) {
        if (fileResults.length > 0) {
            common.log(`\nMatching files (${fileResults.length}):`);
            fileResults.forEach(fr => common.log(formatter.formatFileResult(fr)));
        } else {
            common.log('\nMatching files: 0');
        }
    }
}

exports.Finder = Finder;
