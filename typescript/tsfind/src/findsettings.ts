/*
 * findsettings.ts
 *
 * represents the settings to use when performing the find
 */

'use strict';

import {FileType} from './filetype';
import {FileTypes} from './filetypes';
import {SortBy} from "./sortby";
import {SortUtil} from "./sortutil";
import {StringUtil} from "./stringutil";

export class FindSettings {
    #archivesOnly = false;
    #debug = false;
    excludeHidden = true;
    inArchiveExtensions: string[] = [];
    inArchiveFilePatterns: RegExp[] = [];
    inDirPatterns: RegExp[] = [];
    inExtensions: string[] = [];
    inFilePatterns: RegExp[] = [];
    inFileTypes: FileType[] = [];
    includeArchives = false;
    listDirs = false;
    listFiles = false;
    #maxLastMod: Date | null = null;
    maxSize = 0;
    #minLastMod: Date | null = null;
    minSize = 0;
    outArchiveExtensions: string[] = [];
    outArchiveFilePatterns: RegExp[] = [];
    outDirPatterns: RegExp[] = [];
    outExtensions: string[] = [];
    outFilePatterns: RegExp[] = [];
    outFileTypes: FileType[] = [];
    paths: string[] = [];
    printUsage = false;
    printVersion = false;
    recursive = true;
    sortBy = SortBy.FilePath;
    sortCaseInsensitive = false;
    sortDescending = false;
    verbose = false;

    private static addExtensions(exts: string|string[], arr: string[]): void {
        if (typeof(exts) === 'string') {
            exts.split(/,/).filter(x => x !== '').forEach(x => arr.push(x));
        } else if (exts.constructor === Array) {
            exts.forEach((x: string) => arr.push(x));
        }
    }

    private static addFileTypes(fileTypes: string|string[], arr: FileType[]): void {
        if (typeof(fileTypes) === 'string') {
            fileTypes.split(/,/).filter(ft => ft !== '').
            forEach(ft => arr.push(FileTypes.fromName(ft)));
        } else if (fileTypes.constructor === Array) {
            fileTypes.forEach((ft: string) => arr.push(FileTypes.fromName(ft)));
        }
    }

    private static addPatterns(patterns: string|string[], arr: RegExp[]): void {
        if (typeof(patterns) === 'string') {
            arr.push(new RegExp(patterns));
        } else if (patterns.constructor === Array) {
            patterns.forEach((p: string) => arr.push(new RegExp(p)));
        }
    }

    public addInArchiveExtensions(ext: string|string[]): void {
        FindSettings.addExtensions(ext, this.inArchiveExtensions);
    }

    public addInArchiveFilePatterns(pattern: string|string[]): void {
        FindSettings.addPatterns(pattern, this.inArchiveFilePatterns);
    }

    public addInDirPatterns(pattern: string|string[]): void {
        FindSettings.addPatterns(pattern, this.inDirPatterns);
    }

    public addInExtensions(ext: string|string[]): void {
        FindSettings.addExtensions(ext, this.inExtensions);
    }

    public addInFilePatterns(pattern: string|string[]): void {
        FindSettings.addPatterns(pattern, this.inFilePatterns);
    }

    public addInFileTypes(fileType: string|string[]): void {
        FindSettings.addFileTypes(fileType, this.inFileTypes);
    }

    public addOutArchiveExtensions(ext: string|string[]): void {
        FindSettings.addExtensions(ext, this.outArchiveExtensions);
    }

    public addOutArchiveFilePatterns(pattern: string|string[]): void {
        FindSettings.addPatterns(pattern, this.outArchiveFilePatterns);
    }

    public addOutDirPatterns(pattern: string|string[]): void {
        FindSettings.addPatterns(pattern, this.outDirPatterns);
    }

    public addOutExtensions(ext: string|string[]): void {
        FindSettings.addExtensions(ext, this.outExtensions);
    }

    public addOutFilePatterns(pattern: string|string[]): void {
        FindSettings.addPatterns(pattern, this.outFilePatterns);
    }

    public addOutFileTypes(fileType: string|string[]): void {
        FindSettings.addFileTypes(fileType, this.outFileTypes);
    }

    public get archivesOnly(): boolean {
        return this.#archivesOnly;
    }

    public set archivesOnly(value: boolean) {
        this.#archivesOnly = value;
        if (value) this.includeArchives = value;
    }

    public get debug() {
        return this.#debug;
    }

    public set debug(value: boolean) {
        this.#debug = value;
        if (value) this.verbose = value;
    }

    public get maxLastMod(): Date | null {
        return this.#maxLastMod;
    }

    public set maxLastMod(value: Date | null) {
        this.#maxLastMod = value;
    }

    public maxLastModFromString(value: string) {
        this.#maxLastMod = StringUtil.getDateForString(value);
    }

    public get minLastMod(): Date | null {
        return this.#minLastMod;
    }

    public set minLastMod(value: Date | null) {
        this.#minLastMod = value;
    }

    public minLastModFromString(value: string) {
        this.#minLastMod = StringUtil.getDateForString(value);
    }

    public needStat(): boolean {
        return this.sortBy === SortBy.FileSize ||
            this.sortBy === SortBy.LastMod ||
            this.maxLastMod !== null ||
            this.maxSize > 0 ||
            this.minLastMod !== null ||
            this.minSize > 0;
    }

    public toString(): string {
        return 'FindSettings('
            + 'archivesOnly=' + this.archivesOnly
            + ', debug=' + this.debug
            + ', excludeHidden=' + this.excludeHidden
            + ', ' + StringUtil.listToString('inArchiveExtensions', this.inArchiveExtensions)
            + ', ' + StringUtil.listToString('inArchiveFilePatterns', this.inArchiveFilePatterns)
            + ', ' + StringUtil.listToString('inDirPatterns', this.inDirPatterns)
            + ', ' + StringUtil.listToString('inExtensions', this.inExtensions)
            + ', ' + StringUtil.listToString('inFilePatterns', this.inFilePatterns)
            + ', ' + StringUtil.fileTypesToString('inFileTypes', this.inFileTypes)
            + ', includeArchives=' + this.includeArchives
            + ', listDirs=' + this.listDirs
            + ', listFiles=' + this.listFiles
            + ', ' + StringUtil.dateToString('maxLastMod', this.maxLastMod)
            + ', maxSize=' + this.maxSize
            + ', ' + StringUtil.dateToString('minLastMod', this.minLastMod)
            + ', minSize=' + this.minSize
            + ', ' + StringUtil.listToString('outArchiveExtensions', this.outArchiveExtensions)
            + ', ' + StringUtil.listToString('outArchiveFilePatterns', this.outArchiveFilePatterns)
            + ', ' + StringUtil.listToString('outDirPatterns', this.outDirPatterns)
            + ', ' + StringUtil.listToString('outExtensions', this.outExtensions)
            + ', ' + StringUtil.listToString('outFilePatterns', this.outFilePatterns)
            + ', ' + StringUtil.fileTypesToString('outFileTypes', this.outFileTypes)
            + ', ' + StringUtil.listToString('paths', this.paths)
            + ', printVersion=' + this.printVersion
            + ', recursive=' + this.recursive
            + ', sortBy=' + SortUtil.sortByToName(this.sortBy)
            + ', sortCaseInsensitive=' + this.sortCaseInsensitive
            + ', sortDescending=' + this.sortDescending
            + ', verbose=' + this.verbose
            + ')';
    }
}
