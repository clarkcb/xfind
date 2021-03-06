/*
 * findsettings.ts
 *
 * represents the settings to use when performing the find
 */

'use strict';

import {FileType} from './filetype';
import {FileTypes} from './filetypes';

export class FindSettings {
    archivesOnly = false;
    colorize = true;
    debug = false;
    excludeHidden = true;
    inArchiveExtensions: string[] = [];
    inArchiveFilePatterns: RegExp[] = [];
    inDirPatterns: RegExp[] = [];
    inExtensions: string[] = [];
    inFilePatterns: RegExp[] = [];
    inFileTypes: FileType[] = [];
    listDirs = false;
    listFiles = false;
    outArchiveExtensions: string[] = [];
    outArchiveFilePatterns: RegExp[] = [];
    outDirPatterns: RegExp[] = [];
    outExtensions: string[] = [];
    outFilePatterns: RegExp[] = [];
    outFileTypes: FileType[] = [];
    printUsage = false;
    printVersion = false;
    recursive = true;
    includeArchives = false;
    paths: string[] = [];
    verbose = false;

    private static addExtensions(exts: string|string[], arr: string[]): void {
        if (typeof(exts) === 'string') {
            exts.split(/,/).filter(x => x !== '').forEach(x => arr.push(x));
        } else if (exts.constructor === Array) {
            exts.forEach((x: string) => arr.push(x));
        }
    }

    public addInExtensions(ext: string|string[]): void {
        FindSettings.addExtensions(ext, this.inExtensions);
    }

    public addOutExtensions(ext: string|string[]): void {
        FindSettings.addExtensions(ext, this.outExtensions);
    }

    private static addFileTypes(filetypes: string|string[], arr: FileType[]): void {
        if (typeof(filetypes) === 'string') {
            filetypes.split(/,/).filter(ft => ft !== '').
                forEach(ft => arr.push(FileTypes.fromName(ft)));
        } else if (filetypes.constructor === Array) {
            filetypes.forEach((ft: string) => arr.push(FileTypes.fromName(ft)));
        }
    }

    public addInFileTypes(filetype: string|string[]): void {
        FindSettings.addFileTypes(filetype, this.inFileTypes);
    }

    public addOutFileTypes(filetype: string|string[]): void {
        FindSettings.addFileTypes(filetype, this.outFileTypes);
    }

    private static addPatterns(patterns: string|string[], arr: RegExp[]): void {
        if (typeof(patterns) === 'string') {
            arr.push(new RegExp(patterns));
        } else if (patterns.constructor === Array) {
            patterns.forEach((p: string) => arr.push(new RegExp(p)));
        }
    }

    public addInDirPatterns(pattern: string|string[]): void {
        FindSettings.addPatterns(pattern, this.inDirPatterns);
    }

    public addOutDirPatterns(pattern: string|string[]): void {
        FindSettings.addPatterns(pattern, this.outDirPatterns);
    }

    public addInFilePatterns(pattern: string|string[]): void {
        FindSettings.addPatterns(pattern, this.inFilePatterns);
    }

    public addOutFilePatterns(pattern: string|string[]): void {
        FindSettings.addPatterns(pattern, this.outFilePatterns);
    }

    public addInArchiveExtensions(ext: string|string[]): void {
        FindSettings.addExtensions(ext, this.inArchiveExtensions);
    }

    public addOutArchiveExtensions(ext: string|string[]): void {
        FindSettings.addExtensions(ext, this.outArchiveExtensions);
    }

    public addInArchiveFilePatterns(pattern: string|string[]): void {
        FindSettings.addPatterns(pattern, this.inArchiveFilePatterns);
    }
    public addOutArchiveFilePatterns(pattern: string|string[]): void {
        FindSettings.addPatterns(pattern, this.outArchiveFilePatterns);
    }

    public setArchivesOnly(b: boolean): void {
        this.archivesOnly = b;
        if (b) this.includeArchives = b;
    }

    public setDebug(b: boolean): void {
        this.debug = b;
        if (b) this.verbose = b;
    }

    private static listToString(name: string, lst: string[]|RegExp[]): string {
        let s = `${name}=[`;
        if (lst.length)
            s += `"${lst.join('","')}"`;
        s += ']';
        return s;
    }

    private static fileTypesToString(name: string, fileTypes: FileType[]): string {
        let s = `${name}=[`;
        for (let i=0; i < fileTypes.length; i++) {
            if (i > 0) s += ', ';
            s += `"${FileTypes.toName(fileTypes[i])}"`;
        }
        s += ']';
        return s;
    }

    public toString(): string {
        return 'FindSettings('
            + 'archivesOnly=' + this.archivesOnly
            + ', colorize=' + this.colorize
            + ', debug=' + this.debug
            + ', excludeHidden=' + this.excludeHidden
            + ', ' + FindSettings.listToString('inArchiveExtensions', this.inArchiveExtensions)
            + ', ' + FindSettings.listToString('inArchiveFilePatterns', this.inArchiveFilePatterns)
            + ', ' + FindSettings.listToString('inDirPatterns', this.inDirPatterns)
            + ', ' + FindSettings.listToString('inExtensions', this.inExtensions)
            + ', ' + FindSettings.listToString('inFilePatterns', this.inFilePatterns)
            + ', ' + FindSettings.fileTypesToString('inFileTypes', this.inFileTypes)
            + ', includeArchives=' + this.includeArchives
            + ', listDirs=' + this.listDirs
            + ', listFiles=' + this.listFiles
            + ', ' + FindSettings.listToString('outArchiveExtensions', this.outArchiveExtensions)
            + ', ' + FindSettings.listToString('outArchiveFilePatterns', this.outArchiveFilePatterns)
            + ', ' + FindSettings.listToString('outDirPatterns', this.outDirPatterns)
            + ', ' + FindSettings.listToString('outExtensions', this.outExtensions)
            + ', ' + FindSettings.listToString('outFilePatterns', this.outFilePatterns)
            + ', ' + FindSettings.fileTypesToString('outFileTypes', this.outFileTypes)
            + ', ' + FindSettings.listToString('paths', this.paths)
            + ', printVersion=' + this.printVersion
            + ', recursive=' + this.recursive
            + ', verbose=' + this.verbose
            + ')';
    }
}
