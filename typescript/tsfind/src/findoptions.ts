/*
 * findoptions.js
 *
 * defines the set of find options and provides functionality to define find settings from them
 */

'use strict';

import * as fs from 'fs';

import * as config from './config';
import {FileUtil} from './fileutil';
import {FindError} from "./finderror";
import {FindOption} from './findoption';
import {FindSettings} from './findsettings';
import {SortUtil} from "./sortutil";

interface StringActionMap {
    [key: string]: any
}

interface ArgMapResult {
    err: Error | undefined;
    argMap: {[argName: string]: any};
}

export class FindOptions {
    options: FindOption[];
    argNameMap: {[index: string]:string};
    boolActionMap: StringActionMap;
    stringActionMap: StringActionMap;
    intActionMap: StringActionMap;

    constructor() {
        this.options = [];
        this.argNameMap = {'path': 'path'};

        this.boolActionMap = {
            'archivesonly':
                (b: boolean, settings: FindSettings) => { settings.archivesOnly = b; },
            'colorize':
                (b: boolean, settings: FindSettings) => { settings.colorize = b; },
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
            'nocolorize':
                (b: boolean, settings: FindSettings) => { settings.colorize = !b; },
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
                if (Object.prototype.hasOwnProperty.call(fo, 'short')) {
                    shortArg = fo['short'];
                    this.argNameMap[shortArg] = longArg;
                }
                const desc = fo['desc'];
                this.argNameMap[longArg] = longArg;
                const option = new FindOption(shortArg, longArg, desc);
                this.options.push(option);
            });
        } else throw new Error(`Invalid findoptions file: ${config.FIND_OPTIONS_JSON_PATH}`);
        this.options.sort(FindOptions.optCmp);
    }

    private updateSettingsFromArgMap(settings: FindSettings, argMap: {[argName: string]:any}): Error | undefined {
        let err: Error | undefined = undefined;
        // keys are sorted so that output is consistent across all versions
        const argNames = Object.keys(argMap).sort();
        const invalidNames = argNames.filter(k => !Object.prototype.hasOwnProperty.call(this.argNameMap, k));
        if (invalidNames.length > 0) {
            return new FindError(`Invalid option: ${invalidNames[0]}`);
        }
        for (const argName of argNames) {
            if (err) break;
            if (Object.prototype.hasOwnProperty.call(argMap, argName)) {
                if (argMap[argName] !== undefined && argMap[argName] !== null) {
                    // path is separate because it is not included as an option in findoptions.json
                    let longArg = argName === 'path' ? 'path' : this.argNameMap[argName];
                    if (this.boolActionMap[longArg]) {
                        if (typeof argMap[argName] === 'boolean') {
                            this.boolActionMap[longArg](argMap[argName], settings);
                        } else {
                            err = new FindError(`Invalid value for option: ${argName}`);
                        }
                    } else if (this.stringActionMap[longArg]) {
                        if (typeof argMap[argName] === 'string') {
                            this.stringActionMap[longArg](argMap[argName], settings);
                        } else if (typeof argMap[argName] === 'object' && argMap[argName].constructor === Array) {
                            argMap[argName].forEach((s: string) => {
                                this.stringActionMap[longArg](s, settings);
                            });
                        } else {
                            err = new FindError(`Invalid value for option: ${argName}`);
                        }
                    } else if (this.intActionMap[longArg]) {
                        if (typeof argMap[argName] === 'number') {
                            this.intActionMap[longArg](argMap[argName], settings);
                        } else {
                            err = new FindError(`Invalid value for option: ${argName}`);
                        }
                    } else if (longArg === 'settings-file') {
                        err = this.updateSettingsFromFile(settings, argMap[argName]);
                    } else {
                        err = new FindError(`Invalid option: ${argName}`);
                    }
                } else {
                    err = new FindError(`Missing value for option ${argName}`);
                }
            }
        }
        return err;
    }

    public updateSettingsFromJson(settings: FindSettings, json: string): Error | undefined {
        try {
            const obj = JSON.parse(json);
            return this.updateSettingsFromArgMap(settings, obj);
        } catch (e) {
            if (e instanceof SyntaxError) {
                return new FindError(`Invalid JSON in settings: ${e.message}`);
            } else {
                return new FindError(`Error parsing settings JSON: ${e}`);
            }
        }
    }

    public updateSettingsFromFile(settings: FindSettings, filePath: string): Error | undefined {
        const expandedPath = FileUtil.expandPath(filePath);
        if (fs.existsSync(expandedPath)) {
            if (expandedPath.endsWith('.json')) {
                const json: string = FileUtil.getFileContentsSync(expandedPath);
                return this.updateSettingsFromJson(settings, json);
            }
            return new FindError(`Invalid settings file (must be JSON): ${filePath}`);
        } else {
            return new FindError(`Settings file not found: ${filePath}`);
        }
    }

    private argMapFromArgs(args: string[]): ArgMapResult {
        let err: Error | undefined = undefined;
        let argMap: {[argName: string]:any} = {};

        while(args && !err) {
            let arg = args.shift();
            if (!arg) {
                break;
            }
            if (arg.charAt(0) === '-') {
                let argNames: string[] = [];
                if (arg.length > 1) {
                    if (arg.charAt(1) === '-') {
                        if (arg.length > 2) {
                            arg = arg.substring(2);
                            if (arg.indexOf('=') > -1) {
                                let parts = arg.split('=');
                                if (parts.length > 0) {
                                    arg = parts[0];
                                }
                                if (parts.length > 1) {
                                    args.unshift(parts[1]);
                                }
                            }
                            if (this.argNameMap[arg]) {
                                let longArg = this.argNameMap[arg];
                                argNames.push(longArg);
                            } else {
                                err = new Error(`Invalid option: ${arg}`);
                                break;
                            }
                        } else {
                            err = new Error(`Invalid option: ${arg}`);
                            break;
                        }
                    } else {
                        arg = arg.substring(1);
                        for (const c of arg) {
                            if (this.argNameMap[c]) {
                                let longArg = this.argNameMap[c];
                                argNames.push(longArg);
                            } else {
                                err = new Error(`Invalid option: ${c}`);
                                break;
                            }
                        }
                    }
                } else {
                    err = new Error(`Invalid option: ${arg}`);
                }
                for (const argName of argNames) {
                    if (this.boolActionMap[argName]) {
                        // this.boolActionMap[argName](true, settings);
                        argMap[argName] = true;
                    } else if (this.stringActionMap[argName] || this.intActionMap[argName] || argName === 'settings-file') {
                        if (args.length > 0) {
                            const argValue = args.shift()!;
                            if (this.stringActionMap[argName]) {
                                // err = this.stringActionMap[argName](args.shift(), settings);
                                if (!argMap[argName]) {
                                    argMap[argName] = [];
                                }
                                argMap[argName].push(argValue);
                            } else if (this.intActionMap[argName]) {
                                // err = this.intActionMap[argName](parseInt(args.shift(), 10), settings);
                                argMap[argName] = parseInt(argValue, 10);
                            } else {
                                // err = this.updateSettingsFromFile(args.shift(), settings);
                                argMap['settings-file'] = argValue;
                            }
                        } else {
                            err = new Error(`Missing argument for option ${arg}`);
                            break;
                        }
                    } else {
                        err = new Error(`Invalid option: ${arg}`);
                        break;
                    }
                }

            } else {
                if (!argMap['path']) {
                    argMap['path'] = [];
                }
                argMap['path'].push(arg);
            }
        }
        return { err, argMap };
    }

    public settingsFromArgs(args: string[], cb: (err: Error | undefined, settings: FindSettings) => void): void {
        // let err: Error | undefined = undefined;
        const settings: FindSettings = new FindSettings();
        // default printFiles to true since it's being run from cmd line
        settings.printFiles = true;
        let { err, argMap } = this.argMapFromArgs(args);
        if (!err) {
            err = this.updateSettingsFromArgMap(settings, argMap);
        }
        cb(err, settings);
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

    public usageWithCode(exitCode: number): void {
        console.log(this.getUsageString());
        process.exit(exitCode);
    }

    public usage(): void {
        this.usageWithCode(0);
    }
}
