/*
 * searchsettings.js
 *
 * represents the settings to use when performing the search
 */

var FileTypes = require('./filetypes.js').FileTypes;

function SearchSettings() {
    "use strict";
    let self = this;
    self.archivesOnly = false;
    self.debug = false;
    self.excludeHidden = true;
    self.firstMatch = false;
    self.inArchiveExtensions = [];
    self.inArchiveFilePatterns = [];
    self.inDirPatterns = [];
    self.inExtensions = [];
    self.inFilePatterns = [];
    self.inFileTypes = [];
    self.inLinesAfterPatterns = [];
    self.inLinesBeforePatterns = [];
    self.linesAfter = 0;
    self.linesAfterToPatterns = [];
    self.linesAfterUntilPatterns = [];
    self.linesBefore = 0;
    self.listDirs = false;
    self.listFiles = false;
    self.listLines = false;
    self.maxLineLength = 150;
    self.multilineSearch = false;
    self.outArchiveExtensions = [];
    self.outArchiveFilePatterns = [];
    self.outDirPatterns = [];
    self.outExtensions = [];
    self.outFilePatterns = [];
    self.outFileTypes = [];
    self.outLinesAfterPatterns = [];
    self.outLinesBeforePatterns = [];
    self.printResults = false;
    self.printUsage = false;
    self.printVersion = false;
    self.recursive = true;
    self.searchArchives = false;
    self.searchPatterns = [];
    self.startPath = "";
    self.uniqueLines = false;
    self.verbose = false;

    const addExtensions = function (exts, arr) {
        let xs = exts;
        if (typeof(exts) === 'string') {
            xs = exts.split(/,/);
        }
        xs.filter(x => x !== '').forEach(x => arr.push(x));
    };
    self.addInExtension = function (ext) {
        addExtensions(ext, self.inExtensions);
    };
    self.addOutExtension = function (ext) {
        addExtensions(ext, self.outExtensions);
    };
    const addPatterns = function (patterns, arr) {
        if (typeof(patterns) === 'string') {
            arr.push(new RegExp(patterns));
        } else if (patterns.constructor === Array) {
            patterns.forEach(p => arr.push(new RegExp(p)));
        }
    };
    self.addInDirPattern = function (pattern) {
        addPatterns(pattern, self.inDirPatterns);
    };
    self.addOutDirPattern = function (pattern) {
        addPatterns(pattern, self.outDirPatterns);
    };
    self.addInFilePattern = function (pattern) {
        addPatterns(pattern, self.inFilePatterns);
    };
    self.addOutFilePattern = function (pattern) {
        addPatterns(pattern, self.outFilePatterns);
    };
    self.addSearchPattern = function (pattern) {
        addPatterns(pattern, self.searchPatterns);
    };
    self.addInArchiveExtension = function (ext) {
        addExtensions(ext, self.inArchiveExtensions);
    };
    self.addOutArchiveExtension = function (ext) {
        addExtensions(ext, self.outArchiveExtensions);
    };
    self.addInArchiveFilePattern = function (pattern) {
        addPatterns(pattern, self.inArchiveFilePatterns);
    };
    self.addOutArchiveFilePattern = function (pattern) {
        addPatterns(pattern, self.outArchiveFilePatterns);
    };
    self.addInLinesAfterPattern = function (pattern) {
        addPatterns(pattern, self.inLinesAfterPatterns);
    };
    self.addOutLinesAfterPattern = function (pattern) {
        addPatterns(pattern, self.outLinesAfterPatterns);
    };
    self.addInLinesBeforePattern = function (pattern) {
        addPatterns(pattern, self.inLinesBeforePatterns);
    };
    self.addOutLinesBeforePattern = function (pattern) {
        addPatterns(pattern, self.outLinesBeforePatterns);
    };
    self.addLinesAfterToPattern = function (pattern) {
        addPatterns(pattern, self.linesAfterToPatterns);
    };
    self.addLinesAfterUntilPattern = function (pattern) {
        addPatterns(pattern, self.linesAfterUntilPatterns);
    };

    const addFileTypes = function (filetypes, arr) {
        if (typeof(filetypes) === 'string') {
            filetypes.split(/,/).filter(ft => ft !== '').
                forEach(ft => arr.push(FileTypes.fromName(ft)));
        } else if (filetypes.constructor === Array) {
            filetypes.forEach(ft => arr.push(FileTypes.fromName(ft)));
        }
    };
    self.addInFileType = function (filetype) {
        addFileTypes(filetype, self.inFileTypes);
    };
    self.addOutFileType = function (filetype) {
        addFileTypes(filetype, self.outFileTypes);
    };

    self.setArchivesOnly = function () {
        self.setArchivesOnlyBool(true);
    };

    self.setArchivesOnlyBool = function (b) {
        self.archivesOnly = b;
        if (b) self.searchArchives = b;
    };

    self.setDebug = function () {
        self.setDebugBool(true);
    };

    self.setDebugBool = function (b) {
        self.debug = b;
        if (b) self.verbose = b;
    };

    const listToString = function (name, lst) {
        return `${name}=["${lst.join('","')}"]`;
    };

    self.toString = function () {
        return 'SearchSettings(' +
            'archivesOnly=' + self.archivesOnly +
            ', debug=' + self.debug +
            ', excludeHidden=' + self.excludeHidden +
            ', firstMatch=' + self.firstMatch +
            ', ' + listToString('inArchiveExtensions', self.inArchiveExtensions) +
            ', ' + listToString('inArchiveFilePatterns', self.inArchiveFilePatterns) +
            ', ' + listToString('inDirPatterns', self.inDirPatterns) +
            ', ' + listToString('inExtensions', self.inExtensions) +
            ', ' + listToString('inFilePatterns', self.inFilePatterns) +
            ', ' + listToString('inFileTypes', self.inFileTypes) +
            ', ' + listToString('inLinesAfterPatterns', self.inLinesAfterPatterns) +
            ', ' + listToString('inLinesBeforePatterns', self.inLinesBeforePatterns) +
            ', linesAfter=' + self.linesAfter +
            ', ' + listToString('linesAfterToPatterns', self.linesAfterToPatterns) +
            ', ' + listToString('linesAfterUntilPatterns', self.linesAfterUntilPatterns) +
            ', linesBefore=' + self.linesBefore +
            ', listDirs=' + self.listDirs +
            ', listFiles=' + self.listFiles +
            ', listLines=' + self.listLines +
            ', maxLineLength=' + self.maxLineLength +
            ', multilineSearch=' + self.multilineSearch +
            ', ' + listToString('outArchiveExtensions', self.outArchiveExtensions) +
            ', ' + listToString('outArchiveFilePatterns', self.outArchiveFilePatterns) +
            ', ' + listToString('outDirPatterns', self.outDirPatterns) +
            ', ' + listToString('outExtensions', self.outExtensions) +
            ', ' + listToString('outFilePatterns', self.outFilePatterns) +
            ', ' + listToString('outFileTypes', self.outFileTypes) +
            ', ' + listToString('outLinesAfterPatterns', self.outLinesAfterPatterns) +
            ', ' + listToString('outLinesBeforePatterns', self.outLinesBeforePatterns) +
            ', printResults=' + self.printResults +
            ', printVersion=' + self.printVersion +
            ', recursive=' + self.recursive +
            ', searchArchives=' + self.searchArchives +
            ', ' + listToString('searchPatterns', self.searchPatterns) +
            ',  startPath="' + self.startPath + '"' +
            ', uniqueLines=' + self.uniqueLines +
            ', verbose=' + self.verbose +
            ')';
    };
}

exports.SearchSettings = SearchSettings;
