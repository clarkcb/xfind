/*
 * finder.js
 *
 * performs the finding based on the given FindSettings instance
 */

const assert = require('assert');
const fs = require('fs');
const path = require('path');
const { promisify } = require('util');
const fsLstatAsync = promisify(fs.lstat);
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
                } catch {
                    p = FileUtil.expandPath(p);
                    fs.accessSync(p, fs.constants.F_OK | fs.constants.R_OK);
                }
                let stats = fs.lstatSync(p);
                if (stats.isSymbolicLink()) {
                    assert.ok(this.settings.followSymlinks, startPathDoesNotMatchFindSettings);
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

    emptyOrMatchesAnyElement(s, elements) {
        return elements.length === 0 || this.matchesAnyElement(s, elements);
    }

    emptyOrNotMatchesAnyElement(s, elements) {
        return elements.length === 0 || !this.matchesAnyElement(s, elements);
    }

    emptyOrMatchesAnyPattern(s, patterns) {
        return patterns.length === 0 || this.matchesAnyPattern(s, patterns);
    }

    emptyOrNotMatchesAnyPattern(s, patterns) {
        return patterns.length === 0 || !this.matchesAnyPattern(s, patterns);
    }

    isMatchingDirPathByHidden(dirPath) {
        return this.settings.includeHidden || !FileUtil.isHiddenPath(dirPath);
    }

    isMatchingDirPathByInPatterns(dirPath) {
        return this.settings.inDirPatterns.length === 0
            || this.matchesAnyPattern(dirPath, this.settings.inDirPatterns);
    }

    isMatchingDirPathByOutPatterns(dirPath) {
        return this.settings.outDirPatterns.length === 0
            || !this.matchesAnyPattern(dirPath, this.settings.outDirPatterns);
    }

    isTraversableDirPath(dirPath) {
        return this.isMatchingDirPathByHidden(dirPath)
            && this.isMatchingDirPathByOutPatterns(dirPath);
    }

    isMatchingDirPath(dirPath) {
        return this.isMatchingDirPathByHidden(dirPath)
            && this.isMatchingDirPathByInPatterns(dirPath)
            && this.isMatchingDirPathByOutPatterns(dirPath);
    }

    isNullOrMatchingDirPath(dirPath) {
        if (!dirPath) return true;
        return this.isMatchingDirPath(dirPath);
    }

    isMatchingFileNameByHidden(fileName) {
        return this.settings.includeHidden || !FileUtil.isHiddenName(fileName);
    }

    isMatchingArchiveExtension(ext) {
        return (this.emptyOrMatchesAnyElement(ext, this.settings.inArchiveExtensions)
            && this.emptyOrNotMatchesAnyElement(ext, this.settings.outArchiveExtensions));
    }

    isMatchingArchiveExtensionForFilePath(filePath) {
        if (this.settings.inArchiveExtensions.length || this.settings.outArchiveExtensions.length) {
            const ext = FileUtil.getExtension(path.basename(filePath));
            return this.isMatchingArchiveExtension(ext);
        }
        return true;
    }

    isMatchingArchiveFileName(fileName) {
        return ((this.emptyOrMatchesAnyPattern(fileName, this.settings.inArchiveFilePatterns)) &&
            (this.emptyOrNotMatchesAnyPattern(fileName, this.settings.outArchiveFilePatterns)));
    }

    isMatchingArchiveFileNameForFilePath(filePath) {
        if (this.settings.inArchiveFilePatterns.length || this.settings.outArchiveFilePatterns.length) {
            return this.isMatchingArchiveFileName(path.basename(filePath));
        }
        return true;
    }

    isMatchingArchiveFilePath(filePath) {
        return this.isMatchingArchiveExtensionForFilePath(filePath) &&
            this.isMatchingArchiveFileNameForFilePath(filePath);
    }

    isMatchingArchiveFileResult(fr) {
        return this.isMatchingArchiveFilePath(fr.filePath);
    }


    isMatchingExtension(ext) {
        return (this.emptyOrMatchesAnyElement(ext, this.settings.inExtensions)
            && this.emptyOrNotMatchesAnyElement(ext, this.settings.outExtensions));
    }

    isMatchingExtensionForFilePath(filePath) {
        if (this.settings.inExtensions.length || this.settings.outExtensions.length) {
            let ext = FileUtil.getExtension(path.basename(filePath));
            return this.isMatchingExtension(ext);
        }
        return true;
    }

    isMatchingFileName(fileName) {
        return ((this.emptyOrMatchesAnyPattern(fileName, this.settings.inFilePatterns)) &&
            (this.emptyOrNotMatchesAnyPattern(fileName, this.settings.outFilePatterns)));
    }

    isMatchingFileNameForFilePath(filePath) {
        if (this.settings.inFilePatterns.length || this.settings.outFilePatterns.length) {
            return this.isMatchingFileName(path.basename(filePath));
        }
        return true;
    }

    isMatchingFilePath(filePath) {
        return this.isMatchingExtensionForFilePath(filePath) &&
            this.isMatchingFileNameForFilePath(filePath);
    }

    isMatchingFileType(fileType) {
        return (this.emptyOrMatchesAnyElement(fileType, this.settings.inFileTypes)
            && this.emptyOrNotMatchesAnyElement(fileType, this.settings.outFileTypes));
    }

    isMatchingFileSize(fileSize) {
        return ((this.settings.maxSize === 0 || fileSize <= this.settings.maxSize) &&
            (this.settings.minSize === 0 || fileSize >= this.settings.minSize));
    }

    isMatchingLastMod(lastMod) {
        return ((this.settings.maxLastMod === 0 || lastMod <= this.settings.maxLastMod) &&
            (this.settings.minLastMod === 0 || lastMod >= this.settings.minLastMod));
    }

    isMatchingFileResult(fr) {
        return this.isMatchingFilePath(fr.filePath) &&
            this.isMatchingFileType(fr.fileType) &&
            this.isMatchingFileSize(fr.fileSize) &&
            this.isMatchingLastMod(fr.lastMod);
    }

    filterArchiveFilePathToFileResult(filePath) {
        if (!this.settings.includeArchives && !this.settings.archivesOnly) {
            return null;
        }

        if (!this.isMatchingArchiveFilePath(filePath)) {
            return null;
        }

        return new FileResult(filePath, FileType.ARCHIVE, 0, 0);
    }

    filterRegularFilePathToFileResult(filePath, fileType, stat) {
        if (this.settings.archivesOnly) {
            return null;
        }

        if (!this.isMatchingFilePath(filePath) || !this.isMatchingFileType(fileType)) {
            return null;
        }

        let fileSize = 0;
        let lastMod = 0;
        if (this.settings.needLastMod() || this.settings.needSize()) {
            stat = stat || fs.statSync(filePath);
            if (this.settings.needSize()) fileSize = stat.size;
            if (this.settings.needLastMod()) lastMod = stat.mtime.getTime();

            if (!this.isMatchingFileSize(fileSize) || !this.isMatchingLastMod(lastMod)) {
                return null;
            }
        }

        return new FileResult(filePath, fileType, fileSize, lastMod);
    }

    filterToFileResult(filePath, stat) {
        if (!this.isNullOrMatchingDirPath(path.dirname(filePath))
            || !this.isMatchingFileNameByHidden(path.basename(filePath))) {
            return null;
        }

        const fileType = this.fileTypes.getFileType(path.basename(filePath));
        if (fileType === FileType.ARCHIVE) {
            return this.filterArchiveFilePathToFileResult(filePath);
        }
        return this.filterRegularFilePathToFileResult(filePath, fileType, stat);
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

    async getFileResults(filePath) {
        try {
            fs.accessSync(filePath, fs.constants.F_OK | fs.constants.R_OK);
        } catch {
            filePath = FileUtil.expandPath(filePath);
        }
        let stats = await fsLstatAsync(filePath);
        if (stats.isSymbolicLink() && !this.settings.followSymlinks) {
            throw new FindError(startPathDoesNotMatchFindSettings);
        }
        stats = await fsStatAsync(filePath);
        if (stats.isDirectory()) {
            // if max_depth is zero, we can skip since a directory cannot be a result
            if (this.settings.maxDepth === 0) {
                return [];
            }
            if (this.isTraversableDirPath(filePath)) {
                let maxDepth = this.settings.maxDepth;
                if (!this.settings.recursive) {
                    maxDepth = 1;
                }
                return await this.recGetFileResults(filePath, this.settings.minDepth, maxDepth, 1);
            } else {
                throw new FindError(startPathDoesNotMatchFindSettings);
            }
        } else if (stats.isFile()) {
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
        } else {
            throw new FindError(startPathDoesNotMatchFindSettings);
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
        const dirs = fileResults.map(fr => fr.filePath);
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
