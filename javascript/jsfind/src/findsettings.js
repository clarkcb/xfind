/*
 * findsettings.js
 *
 * represents the settings to use when performing the find
 */

const {FileTypes} = require('./filetypes');

class FindSettings {
    constructor() {
        this.archivesOnly = false;
        this.debug = false;
        this.excludeHidden = true;
        this.inArchiveExtensions = [];
        this.inArchiveFilePatterns = [];
        this.inDirPatterns = [];
        this.inExtensions = [];
        this.inFilePatterns = [];
        this.inFileTypes = [];
        this.includeArchives = false;
        this.listDirs = false;
        this.listFiles = false;
        this.outArchiveExtensions = [];
        this.outArchiveFilePatterns = [];
        this.outDirPatterns = [];
        this.outExtensions = [];
        this.outFilePatterns = [];
        this.outFileTypes = [];
        this.paths = [];
        this.printUsage = false;
        this.printVersion = false;
        this.recursive = true;
        this.verbose = false;
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

    addFileTypes(filetypes, arr) {
        if (typeof(filetypes) === 'string') {
            filetypes.split(/,/).filter(ft => ft !== '').
            forEach(ft => arr.push(FileTypes.fromName(ft)));
        } else if (filetypes.constructor === Array) {
            filetypes.forEach(ft => arr.push(FileTypes.fromName(ft)));
        }
    }

    addInFileTypes(filetype) {
        this.addFileTypes(filetype, this.inFileTypes);
    }

    addOutFileTypes(filetype) {
        this.addFileTypes(filetype, this.outFileTypes);
    }

    setArchivesOnly(b = true) {
        this.archivesOnly = b;
        if (b) this.includeArchives = b;
    }

    setDebug(b = true) {
        this.debug = b;
        if (b) this.verbose = b;
    }

    listToString(name, lst) {
        if (lst.length) return `${name}=["${lst.join('","')}"]`;
        return `${name}=[]`;
    }

    fileTypesToString(name, fileTypes) {
        if (fileTypes.length) {
            var s = `${name}=[`;
            for (var i=0; i < fileTypes.length; i++) {
                if (i > 0) s += ', ';
                s += '"' + FileTypes.toName(fileTypes[i]) + '"';
            }
            s += ']';
            return s;
        }
        return `${name}=[]`;
    }

    toString() {
        return 'FindSettings(' +
            'archivesOnly=' + this.archivesOnly +
            ', debug=' + this.debug +
            ', excludeHidden=' + this.excludeHidden +
            ', ' + this.listToString('inArchiveExtensions', this.inArchiveExtensions) +
            ', ' + this.listToString('inArchiveFilePatterns', this.inArchiveFilePatterns) +
            ', ' + this.listToString('inDirPatterns', this.inDirPatterns) +
            ', ' + this.listToString('inExtensions', this.inExtensions) +
            ', ' + this.listToString('inFilePatterns', this.inFilePatterns) +
            ', ' + this.fileTypesToString('inFileTypes', this.inFileTypes) +
            ', includeArchives=' + this.includeArchives +
            ', listDirs=' + this.listDirs +
            ', listFiles=' + this.listFiles +
            ', ' + this.listToString('outArchiveExtensions', this.outArchiveExtensions) +
            ', ' + this.listToString('outArchiveFilePatterns', this.outArchiveFilePatterns) +
            ', ' + this.listToString('outDirPatterns', this.outDirPatterns) +
            ', ' + this.listToString('outExtensions', this.outExtensions) +
            ', ' + this.listToString('outFilePatterns', this.outFilePatterns) +
            ', ' + this.fileTypesToString('outFileTypes', this.outFileTypes) +
            ', ' + this.listToString('paths', this.paths) +
            ', printVersion=' + this.printVersion +
            ', recursive=' + this.recursive +
            ', verbose=' + this.verbose +
            ')';
    }
}

exports.FindSettings = FindSettings;
