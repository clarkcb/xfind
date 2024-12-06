/*
 * findoptions.js
 *
 * defines the set of find options and provides functionality to define find settings from them
 */

'use strict';

import * as fs from 'fs';

import * as config from './config';
import {FileUtil} from './fileutil';
import {FindOption} from './findoption';
import {FindSettings} from './findsettings';
import {SortUtil} from "./sortutil";

interface StringActionMap {
    [key: string]: any
}

export class FindOptions {
    options: FindOption[];
    argNameMap: {[index: string]:string};
    boolActionMap: StringActionMap;
    stringActionMap: StringActionMap;
    intActionMap: StringActionMap;

    constructor() {
        this.options = [];
        this.argNameMap = {};

        this.boolActionMap = {
            'archivesonly':
                (b: boolean, settings: FindSettings) => { settings.archivesOnly = b; },
            'debug':
                (b: boolean, settings: FindSettings) => { settings.debug = b; },
            'excludearchives':
                (b: boolean, settings: FindSettings) => { settings.includeArchives = !b; },
            'excludehidden':
                (b: boolean, settings: FindSettings) => { settings.includeHidden = !b; },
            'followsymlinks':
                (b: boolean, settings: FindSettings) => { settings.followSymlinks = b; },
            'includearchives':
                (b: boolean, settings: FindSettings) => { settings.includeArchives = b; },
            'includehidden':
                (b: boolean, settings: FindSettings) => { settings.includeHidden = b; },
            'help':
                (b: boolean, settings: FindSettings) => { settings.printUsage = b; },
            'nofollowsymlinks':
                (b: boolean, settings: FindSettings) => { settings.followSymlinks = !b; },
            'noprintdirs':
                (b: boolean, settings: FindSettings) => { settings.printDirs = !b; },
            'noprintfiles':
                (b: boolean, settings: FindSettings) => { settings.printFiles = !b; },
            'norecursive':
                (b: boolean, settings: FindSettings) => { settings.recursive = !b; },
            'printdirs':
                (b: boolean, settings: FindSettings) => { settings.printDirs = b; },
            'printfiles':
                (b: boolean, settings: FindSettings) => { settings.printFiles = b; },
            'recursive':
                (b: boolean, settings: FindSettings) => { settings.recursive = b; },
            'sort-ascending':
                (b: boolean, settings: FindSettings) => { settings.sortDescending = !b; },
            'sort-caseinsensitive':
                (b: boolean, settings: FindSettings) => { settings.sortCaseInsensitive = b; },
            'sort-casesensitive':
                (b: boolean, settings: FindSettings) => { settings.sortCaseInsensitive = !b; },
            'sort-descending':
                (b: boolean, settings: FindSettings) => { settings.sortDescending = b; },
            'verbose':
                (b: boolean, settings: FindSettings) => { settings.verbose = b; },
            'version':
                (b: boolean, settings: FindSettings) => { settings.printVersion = b; }
        };

        this.stringActionMap = {
            'in-archiveext':
                (s: string, settings: FindSettings) => { settings.addInArchiveExtensions(s); },
            'in-archivefilepattern':
                (s: string, settings: FindSettings) => { settings.addInArchiveFilePatterns(s); },
            'in-dirpattern':
                (s: string, settings: FindSettings) => { settings.addInDirPatterns(s); },
            'in-ext':
                (s: string, settings: FindSettings) => { settings.addInExtensions(s); },
            'in-filepattern':
                (s: string, settings: FindSettings) => { settings.addInFilePatterns(s); },
            'in-filetype':
                (s: string, settings: FindSettings) => { settings.addInFileTypes(s); },
            'maxlastmod':
                (s: string, settings: FindSettings) => { settings.maxLastModFromString(s); },
            'minlastmod':
                (s: string, settings: FindSettings) => { settings.minLastModFromString(s); },
            'out-dirpattern':
                (s: string, settings: FindSettings) => { settings.addOutDirPatterns(s); },
            'out-archiveext':
                (s: string, settings: FindSettings) => { settings.addOutArchiveExtensions(s); },
            'out-archivefilepattern':
                (s: string, settings: FindSettings) => { settings.addOutArchiveFilePatterns(s); },
            'out-ext':
                (s: string, settings: FindSettings) => { settings.addOutExtensions(s); },
            'out-filepattern':
                (s: string, settings: FindSettings) => { settings.addOutFilePatterns(s); },
            'out-filetype':
                (s: string, settings: FindSettings) => { settings.addOutFileTypes(s); },
            'path':
                (s: string, settings: FindSettings) => { settings.paths.push(s); },
            'settings-file':
                (s: string, settings: FindSettings) => { this.settingsFromFile(s, settings); },
            'sort-by':
                (s: string, settings: FindSettings) => { settings.sortBy = SortUtil.nameToSortBy(s); }
        };

        this.intActionMap = {
            'maxdepth':
                (i: number, settings: FindSettings) => { settings.maxDepth = i; },
            'maxsize':
                (i: number, settings: FindSettings) => { settings.maxSize = i; },
            'mindepth':
                (i: number, settings: FindSettings) => { settings.minDepth = i; },
            'minsize':
                (i: number, settings: FindSettings) => { settings.minSize = i; },
        };

        this.setOptionsFromJsonFile();
    }

    private static optCmp(o1: FindOption, o2: FindOption) {
        const a: string = o1.sortArg;
        const b: string = o2.sortArg;
        return a.localeCompare(b);
    }

    // setOptionsFromJsonFile
    private setOptionsFromJsonFile(): void {
        const json = FileUtil.getFileContentsSync(config.FIND_OPTIONS_JSON_PATH);
        const obj = JSON.parse(json);
        if (Object.prototype.hasOwnProperty.call(obj, 'findoptions') && Array.isArray(obj['findoptions'])) {
            obj['findoptions'].forEach(fo => {
                const longArg = fo['long'];
                let shortArg = '';
                if (Object.prototype.hasOwnProperty.call(fo, 'short'))
                    shortArg = fo['short'];
                const desc = fo['desc'];
                this.argNameMap[longArg] = longArg;
                if (shortArg) this.argNameMap[shortArg] = longArg;
                const option = new FindOption(shortArg, longArg, desc);
                this.options.push(option);
            });
        } else throw new Error(`Invalid findoptions file: ${config.FIND_OPTIONS_JSON_PATH}`);
        this.options.sort(FindOptions.optCmp);
    }

    private settingsFromFile(filePath: string, settings: FindSettings): Error | undefined {
        if (fs.existsSync(filePath)) {
            const json: string = FileUtil.getFileContentsSync(filePath);
            return this.settingsFromJson(json, settings);
        } else {
            return new Error('Settings file not found');
        }
    }

    public settingsFromJson(json: string, settings: FindSettings): Error | undefined {
        let err: Error | undefined = undefined;
        const obj = JSON.parse(json);
        for (const k in obj) {
            if (err) break;
            if (Object.prototype.hasOwnProperty.call(obj, k)) {
                if (this.boolActionMap[k]) {
                    this.boolActionMap[k](obj[k], settings);
                } else if (this.stringActionMap[k]) {
                    if (obj[k]) {
                        err = this.stringActionMap[k](obj[k], settings);
                    } else {
                        err = new Error(`Missing argument for option ${k}`);
                    }
                } else if (this.intActionMap[k]) {
                    this.intActionMap[k](obj[k], settings);
                } else {
                    err = new Error(`Invalid option: ${k}`);
                }
            }
        }
        return err;
    }

    public settingsFromArgs(args: string[], cb: (err: Error | undefined, settings: FindSettings) => void): void {
        let err: Error | undefined = undefined;
        const settings: FindSettings = new FindSettings();
        // default printFiles to true since it's being run from cmd line
        settings.printFiles = true;
        while(args && !err) {
            let arg: string = args.shift() || '';
            if (!arg) {
                break;
            }
            if (arg.charAt(0) === '-') {
                while (arg && arg.charAt(0) === '-') {
                    arg = arg.substring(1);
                }
                const longArg = this.argNameMap[arg];
                if (this.boolActionMap[longArg]) {
                    this.boolActionMap[longArg](true, settings);
                } else if (this.stringActionMap[longArg] || this.intActionMap[longArg]) {
                    if (args.length > 0) {
                        if (this.stringActionMap[longArg]) {
                            this.stringActionMap[longArg](args.shift(), settings);
                        } else {
                            this.intActionMap[longArg](parseInt(args.shift()!, 10), settings);
                        }
                    } else {
                        err = new Error(`Missing argument for option ${arg}`);
                    }
                } else {
                    err = new Error(`Invalid option: ${arg}`);
                }
            } else {
                settings.paths.push(arg);
            }
        }
        cb(err, settings);
    }

    public usage(): void {
        this.usageWithCode(0);
    }

    public usageWithCode(exitCode: number): void {
        console.log(this.getUsageString());
        process.exit(exitCode);
    }

    private getUsageString(): string {
        let usage: string = 'Usage:\n tsfind [options] <path> [<path> ...]' +
            '\n\nOptions:\n';
        const optStrings: string[] = [];
        const optDescs: string[] = [];
        let longest = 0;
        this.options.forEach((opt: FindOption) => {
            let optString = ' ';
            if (opt.shortArg)
                optString += '-' + opt.shortArg + ',';
            optString += '--' + opt.longArg;
            if (optString.length > longest)
                longest = optString.length;
            optStrings.push(optString);
            optDescs.push(opt.desc);
        });
        for (let i = 0; i < optStrings.length; i++) {
            let os: string = optStrings[i];
            while (os.length < longest)
                os += ' ';
            usage += os + '  ' + optDescs[i] + '\n';
        }
        return usage;
    }
}
