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

interface StringOptionMap {
    [key: string]: FindOption
}

interface StringActionMap {
    [key: string]: any
}

export class FindOptions {
    // the list of FindOption objects (populated by setOptionsFromXml)
    options: FindOption[];
    argNameMap: {[index: string]:string};
    argMap: StringOptionMap;
    flagMap: StringOptionMap;
    argActionMap: StringActionMap;
    boolFlagActionMap: StringActionMap;

    constructor() {
        this.options = [];
        this.argNameMap = {};
        this.argMap = {};
        this.flagMap = {};

        this.argActionMap = {
            'in-archiveext':
                (x: string, settings: FindSettings) => { settings.addInArchiveExtensions(x); },
            'in-archivefilepattern':
                (x: string, settings: FindSettings) => { settings.addInArchiveFilePatterns(x); },
            'in-dirpattern':
                (x: string, settings: FindSettings) => { settings.addInDirPatterns(x); },
            'in-ext':
                (x: string, settings: FindSettings) => { settings.addInExtensions(x); },
            'in-filepattern':
                (x: string, settings: FindSettings) => { settings.addInFilePatterns(x); },
            'in-filetype':
                (x: string, settings: FindSettings) => { settings.addInFileTypes(x); },
            'maxdepth':
                (x: string, settings: FindSettings) => { settings.maxDepth = parseInt(x, 10); },
            'maxlastmod':
                (x: string, settings: FindSettings) => { settings.maxLastModFromString(x); },
            'maxsize':
                (x: string, settings: FindSettings) => { settings.maxSize = parseInt(x, 10); },
            'mindepth':
                (x: string, settings: FindSettings) => { settings.minDepth = parseInt(x, 10); },
            'minlastmod':
                (x: string, settings: FindSettings) => { settings.minLastModFromString(x); },
            'minsize':
                (x: string, settings: FindSettings) => { settings.minSize = parseInt(x, 10); },
            'out-dirpattern':
                (x: string, settings: FindSettings) => { settings.addOutDirPatterns(x); },
            'out-archiveext':
                (x: string, settings: FindSettings) => { settings.addOutArchiveExtensions(x); },
            'out-archivefilepattern':
                (x: string, settings: FindSettings) => { settings.addOutArchiveFilePatterns(x); },
            'out-ext':
                (x: string, settings: FindSettings) => { settings.addOutExtensions(x); },
            'out-filepattern':
                (x: string, settings: FindSettings) => { settings.addOutFilePatterns(x); },
            'out-filetype':
                (x: string, settings: FindSettings) => { settings.addOutFileTypes(x); },
            'path':
                (x: string, settings: FindSettings) => { settings.paths.push(x); },
            'settings-file':
                (x: string, settings: FindSettings) => { this.settingsFromFile(x, settings); },
            'sort-by':
                (x: string, settings: FindSettings) => { settings.sortBy = SortUtil.nameToSortBy(x); }
        };

        this.boolFlagActionMap = {
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

        this.setOptionsFromJsonFile();
    }

    private static optCmp(o1: FindOption, o2: FindOption) {
        const a: string = o1.sortArg;
        const b: string = o2.sortArg;
        return a.localeCompare(b);
    }

    // setOptionsFromJsonFile
    private setOptionsFromJsonFile(): void {
        const json = FileUtil.getFileContentsSync(config.FINDOPTIONSJSONPATH);
        const obj = JSON.parse(json);
        if (Object.prototype.hasOwnProperty.call(obj, 'findoptions') && Array.isArray(obj['findoptions'])) {
            obj['findoptions'].forEach(so => {
                const longArg = so['long'];
                let shortArg = '';
                if (Object.prototype.hasOwnProperty.call(so, 'short'))
                    shortArg = so['short'];
                const desc = so['desc'];
                this.argNameMap[longArg] = longArg;
                if (shortArg) this.argNameMap[shortArg] = longArg;
                const option = new FindOption(shortArg, longArg, desc);
                this.options.push(option);
                if (this.argActionMap[longArg]) {
                    this.argMap[longArg] = option;
                    if (shortArg) this.argMap[shortArg] = option;
                } else if (this.boolFlagActionMap[longArg]) {
                    this.flagMap[longArg] = option;
                    if (shortArg) this.flagMap[shortArg] = option;
                }
            });
        } else throw new Error("Invalid findoptions file: " + config.FINDOPTIONSJSONPATH);
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
                if (this.argMap[k]) {
                    if (obj[k]) {
                        err = this.argActionMap[k](obj[k], settings);
                    } else {
                        err = new Error("Missing argument for option "+k);
                    }
                } else if (this.boolFlagActionMap[k]) {
                    this.boolFlagActionMap[k](obj[k], settings);
                } else if (k === 'path') {
                    settings.paths.push(obj[k]);
                } else {
                    err = new Error("Invalid option: "+k);
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
                if (this.argMap[longArg]) {
                    if (args.length > 0) {
                        err = this.argActionMap[longArg](args.shift(), settings);
                    } else {
                        err = new Error("Missing argument for option " + arg);
                    }
                } else if (this.flagMap[longArg]) {
                    this.boolFlagActionMap[longArg](true, settings);
                } else {
                    err = new Error("Invalid option: " + arg);
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
