/*
 * findoptions.js
 *
 * defines the set of find options and provides functionality to define find settings from them
 */

'use strict';

import * as config from './config';
import {ArgToken} from "./argtoken";
import {ArgTokenizer} from './argtokenizer';
import {ArgTokenType} from "./argtokentype";
import {FileUtil} from './fileutil';
import {FindError} from "./finderror";
import {FindOption} from './findoption';
import {FindSettings} from './findsettings';
import {SortUtil} from "./sortutil";

interface StringActionMap {
    [key: string]: any
}

export class FindOptions {
    options: FindOption[];
    boolActionMap: StringActionMap;
    stringActionMap: StringActionMap;
    intActionMap: StringActionMap;
    argTokenizer: ArgTokenizer;

    constructor() {
        this.options = [];

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
        this.argTokenizer = new ArgTokenizer(this.options);
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
                let argType = ArgTokenType.Unknown;
                if (this.boolActionMap[longArg]) {
                    argType = ArgTokenType.Bool;
                } else if (this.stringActionMap[longArg]) {
                    argType = ArgTokenType.Str;
                } else if (this.intActionMap[longArg]) {
                    argType = ArgTokenType.Int;
                }
                let shortArg = '';
                if (Object.prototype.hasOwnProperty.call(fo, 'short')) {
                    shortArg = fo['short'];
                }
                const desc = fo['desc'];
                this.options.push(new FindOption(shortArg, longArg, desc, argType));
            });
            // add the path option (not in the json file)
            this.options.push(new FindOption('', 'path', '', ArgTokenType.Str));
        } else throw new Error(`Invalid findoptions file: ${config.FIND_OPTIONS_JSON_PATH}`);
        this.options.sort(FindOptions.optCmp);
    }

    private updateSettingsFromArgTokens(settings: FindSettings, argTokens: ArgToken[]): Error | undefined {
        let err: Error | undefined = undefined;
        for (const argToken of argTokens) {
            if (err) break;
            if (argToken.type === ArgTokenType.Bool) {
                if (typeof argToken.value === 'boolean') {
                    this.boolActionMap[argToken.name](argToken.value, settings);
                } else {
                    err = new FindError(`Invalid value for option: ${argToken}`);
                }
            } else if (argToken.type === ArgTokenType.Str) {
                if (argToken.name === 'settings-file') {
                    err = this.updateSettingsFromFile(settings, argToken.value);
                } else if (typeof argToken.value === 'string') {
                    this.stringActionMap[argToken.name](argToken.value, settings);
                } else if (typeof argToken.value === 'object' && Array.isArray(argToken.value)) {
                    argToken.value.forEach(s => {
                        if (typeof s === 'string') {
                            this.stringActionMap[argToken.name](s, settings);
                        } else {
                            err = new FindError(`Invalid value for option: ${argToken}`);
                        }
                    });
                } else {
                    err = new FindError(`Invalid value for option: ${argToken}`);
                }
            } else if (argToken.type === ArgTokenType.Int) {
                if (typeof argToken.value === 'number') {
                    this.intActionMap[argToken.name](argToken.value, settings);
                } else {
                    err = new FindError(`Invalid value for option: ${argToken}`);
                }
            } else {
                err = new FindError(`Invalid option: ${argToken}`);
            }
        }
        return err;
    }

    public updateSettingsFromJson(settings: FindSettings, json: string): Error | undefined {
        let { err, argTokens } = this.argTokenizer.tokenizeJson(json);
        if (!err) {
            err = this.updateSettingsFromArgTokens(settings, argTokens);
        }
        return err;
    }

    public updateSettingsFromFile(settings: FindSettings, filePath: string): Error | undefined {
        let { err, argTokens } = this.argTokenizer.tokenizeFile(filePath);
        if (!err) {
            err = this.updateSettingsFromArgTokens(settings, argTokens);
        }
        return err;
    }

    public updateSettingsFromArgs(settings: FindSettings, args: string[]): Error | undefined {
        let { err, argTokens } = this.argTokenizer.tokenizeArgs(args);
        if (!err) {
            err = this.updateSettingsFromArgTokens(settings, argTokens);
        }
        return err;
    }

    public settingsFromArgs(args: string[], cb: (err: Error | undefined, settings: FindSettings) => void): void {
        const settings: FindSettings = new FindSettings();
        // default printFiles to true since it's being run from cmd line
        settings.printFiles = true;
        let err = this.updateSettingsFromArgs(settings, args);
        cb(err, settings);
    }

    private getUsageString(): string {
        let usage: string = 'Usage:\n tsfind [options] <path> [<path> ...]' +
            '\n\nOptions:\n';
        const optStrings: string[] = [];
        const optDescs: string[] = [];
        let longest = 0;
        this.options.forEach((opt: FindOption) => {
            if (opt.longArg === 'path')
                return;
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
