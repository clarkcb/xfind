/*
 * findsettings.ts
 *
 * represents the settings to use when performing the find
 */

'use strict';

import {Color} from "./color";
import {FileType} from './filetype';
import {FileTypes} from './filetypes';
import {SortBy} from "./sortby";
import {StringUtil} from "./stringutil";

export class FindSettings {
    #archivesOnly = false;
    colorize = true;
    #debug = false;
    defaultFiles = true;
    dirColor = Color.CYAN;
    extColor = Color.YELLOW;
    fileColor = Color.MAGENTA;
    followSymlinks = false;
    inArchiveExtensions: string[] = [];
    inArchiveFilePatterns: RegExp[] = [];
    inDirPatterns: RegExp[] = [];
    inExtensions: string[] = [];
    inFilePatterns: RegExp[] = [];
    inFileTypes: FileType[] = [];
    includeArchives = false;
    includeHidden = false;
    maxDepth = -1;
    maxLastMod = 0;
    maxSize = 0;
    minDepth = -1;
    minLastMod = 0;
    minSize = 0;
    outArchiveExtensions: string[] = [];
    outArchiveFilePatterns: RegExp[] = [];
    outDirPatterns: RegExp[] = [];
    outExtensions: string[] = [];
    outFilePatterns: RegExp[] = [];
    outFileTypes: FileType[] = [];
    paths: string[] = [];
    printDirs = false;
    printFiles = false;
    printUsage = false;
    printVersion = false;
    recursive = true;
    sortBy = SortBy.FilePath;
    sortCaseInsensitive = false;
    sortDescending = false;
    verbose = false;

    static addExtensions(exts: string|string[], arr: string[]): void {
        if (typeof(exts) === 'string') {
            exts.split(/,/).filter(x => x !== '').forEach(x => arr.push(x));
        } else if (exts.constructor === Array) {
            exts.forEach((x: string) => arr.push(x));
        }
    }

    static addFileTypes(fileTypes: string|string[], arr: FileType[]): void {
        if (typeof(fileTypes) === 'string') {
            fileTypes.split(/,/).filter(ft => ft !== '').
            forEach(ft => arr.push(FileTypes.fromName(ft)));
        } else if (fileTypes.constructor === Array) {
            fileTypes.forEach((ft: string) => arr.push(FileTypes.fromName(ft)));
        }
    }

    static addPatterns(patterns: string|string[], arr: RegExp[]): void {
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

    public maxLastModFromString(value: string) {
        this.maxLastMod = StringUtil.getTimestampForString(value);
    }

    public minLastModFromString(value: string) {
        this.minLastMod = StringUtil.getTimestampForString(value);
    }

    public needLastMod(): boolean {
        return this.sortBy === SortBy.LastMod ||
            this.maxLastMod > 0 ||
            this.minLastMod > 0;
    }

    public needSize(): boolean {
        return this.sortBy === SortBy.FileSize ||
            this.maxSize > 0 ||
            this.minSize > 0;
    }

    public needStat(): boolean {
        return this.needLastMod() || this.needSize();
    }

    public toString(): string {
        let propStrings = [];
        let propKeys = Reflect.ownKeys(this);
        propKeys.sort();
        for (let p of propKeys) {
            let name = p.toString();
            let value: any = Reflect.get(this, p);
            if (name.startsWith('_')) {
                name = name.substring(1);
            }
            if (value instanceof Array) {
                if (name.endsWith('Patterns')) {
                    propStrings.push(StringUtil.patternListToString(name, value));
                } else if (name.endsWith('FileTypes')) {
                    propStrings.push(FileTypes.fileTypesToString(name, value));
                } else {
                    propStrings.push(StringUtil.stringListToString(name, value));
                }
            } else if (name.endsWith('LastMod')) {
                propStrings.push(StringUtil.timestampToString(name, value));
            } else {
                propStrings.push(`${name}=${value}`);
            }
        }
        let propString = propStrings.join(', ');
        return `${this.constructor.name}(${propString})`;
    }
}
