/*
 * findsettings.js
 *
 * represents the settings to use when performing the find
 */

const {FileTypes} = require('./filetypes');
const {SortBy, sortByToName} = require("./sortby");
const StringUtil = require('./stringutil');

class FindSettings {
    _archivesOnly = false;
    colorize = true;
    _debug = false;
    followSymlinks = false;
    inArchiveExtensions = [];
    inArchiveFilePatterns = [];
    inDirPatterns = [];
    inExtensions = [];
    inFilePatterns = [];
    inFileTypes = [];
    includeArchives = false;
    includeHidden = false;
    maxDepth = -1;
    maxLastMod = 0;
    maxSize = 0;
    minDepth = -1;
    minLastMod = 0;
    minSize = 0;
    outArchiveExtensions = [];
    outArchiveFilePatterns = [];
    outDirPatterns = [];
    outExtensions = [];
    outFilePatterns = [];
    outFileTypes = [];
    paths = [];
    printDirs = false;
    printFiles = false;
    printUsage = false;
    printVersion = false;
    recursive = true;
    sortBy = SortBy.FILEPATH;
    sortCaseInsensitive = false;
    sortDescending = false;
    verbose = false;

    constructor() {
    }

    get archivesOnly() {
        return this._archivesOnly;
    }

    set archivesOnly(value) {
        this._archivesOnly = value;
        if (value) this.includeArchives = value;
    }

    get debug() {
        return this._debug;
    }

    set debug(value) {
        this._debug = value;
        if (value) this.verbose = value;
    }

    addExtensions(exts, arr) {
        let xs = exts;
        if (typeof(exts) === 'string') {
            xs = exts.split(/,/);
        }
        xs.filter(x => x !== '').forEach(x => arr.push(x));
    }

    addInExtensions(ext) {
        this.addExtensions(ext, this.inExtensions);
    }

    addOutExtensions(ext) {
        this.addExtensions(ext, this.outExtensions);
    }

    addPatterns(patterns, arr) {
        if (typeof(patterns) === 'string') {
            arr.push(new RegExp(patterns));
        } else if (patterns.constructor === Array) {
            patterns.forEach(p => arr.push(new RegExp(p)));
        }
    }

    addInDirPatterns(pattern) {
        this.addPatterns(pattern, this.inDirPatterns);
    }

    addOutDirPatterns(pattern) {
        this.addPatterns(pattern, this.outDirPatterns);
    }

    addInFilePatterns(pattern) {
        this.addPatterns(pattern, this.inFilePatterns);
    }

    addOutFilePatterns(pattern) {
        this.addPatterns(pattern, this.outFilePatterns);
    }

    addInArchiveExtensions(ext) {
        this.addExtensions(ext, this.inArchiveExtensions);
    }

    addOutArchiveExtensions(ext) {
        this.addExtensions(ext, this.outArchiveExtensions);
    }

    addInArchiveFilePatterns(pattern) {
        this.addPatterns(pattern, this.inArchiveFilePatterns);
    }

    addOutArchiveFilePatterns(pattern) {
        this.addPatterns(pattern, this.outArchiveFilePatterns);
    }

    addFileTypes(fileTypes, arr) {
        if (typeof(fileTypes) === 'string') {
            fileTypes.split(/,/).filter(ft => ft !== '').
            forEach(ft => arr.push(FileTypes.fromName(ft)));
        } else if (fileTypes.constructor === Array) {
            fileTypes.forEach(ft => arr.push(FileTypes.fromName(ft)));
        }
    }

    addInFileTypes(fileType) {
        this.addFileTypes(fileType, this.inFileTypes);
    }

    addOutFileTypes(fileType) {
        this.addFileTypes(fileType, this.outFileTypes);
    }

    maxLastModFromString(s) {
        this.maxLastMod = StringUtil.getTimestampForString(s);
    }

    minLastModFromString(s) {
        this.minLastMod = StringUtil.getTimestampForString(s);
    }

    needLastMod() {
        return this.sortBy === SortBy.LASTMOD ||
          this.maxLastMod > 0 ||
          this.minLastMod > 0;
    }

    needSize() {
        return this.sortBy === SortBy.FILESIZE ||
          this.maxSize > 0 ||
          this.minSize > 0;
    }

    toString() {
        return 'FindSettings(' +
          'archivesOnly=' + this.archivesOnly +
          ', colorize=' + this.colorize +
          ', debug=' + this.debug +
          ', followSymlinks=' + this.followSymlinks +
          ', ' + StringUtil.stringListToString('inArchiveExtensions', this.inArchiveExtensions) +
          ', ' + StringUtil.patternListToString('inArchiveFilePatterns', this.inArchiveFilePatterns) +
          ', ' + StringUtil.patternListToString('inDirPatterns', this.inDirPatterns) +
          ', ' + StringUtil.stringListToString('inExtensions', this.inExtensions) +
          ', ' + StringUtil.patternListToString('inFilePatterns', this.inFilePatterns) +
          ', ' + FileTypes.fileTypesToString('inFileTypes', this.inFileTypes) +
          ', includeArchives=' + this.includeArchives +
          ', includeHidden=' + this.includeHidden +
          ', maxDepth=' + this.maxDepth +
          ', ' + StringUtil.timestampToString('maxLastMod', this.maxLastMod) +
          ', maxSize=' + this.maxSize +
          ', minDepth=' + this.minDepth +
          ', ' + StringUtil.timestampToString('minLastMod', this.minLastMod) +
          ', minSize=' + this.minSize +
          ', ' + StringUtil.stringListToString('outArchiveExtensions', this.outArchiveExtensions) +
          ', ' + StringUtil.patternListToString('outArchiveFilePatterns', this.outArchiveFilePatterns) +
          ', ' + StringUtil.patternListToString('outDirPatterns', this.outDirPatterns) +
          ', ' + StringUtil.stringListToString('outExtensions', this.outExtensions) +
          ', ' + StringUtil.patternListToString('outFilePatterns', this.outFilePatterns) +
          ', ' + FileTypes.fileTypesToString('outFileTypes', this.outFileTypes) +
          ', ' + StringUtil.stringListToString('paths', this.paths) +
          ', printDirs=' + this.printDirs +
          ', printFiles=' + this.printFiles +
          ', printUsage=' + this.printUsage +
          ', printVersion=' + this.printVersion +
          ', recursive=' + this.recursive +
          ', sortBy=' + sortByToName(this.sortBy) +
          ', sortCaseInsensitive=' + this.sortCaseInsensitive +
          ', sortDescending=' + this.sortDescending +
          ', verbose=' + this.verbose +
          ')';
    }
}

exports.FindSettings = FindSettings;
