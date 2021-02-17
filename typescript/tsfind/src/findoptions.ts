/*
 * findoptions.js
 *
 * defines the set of find options and provides functionality to define find settings from them
 */

"use strict";

import * as config from './config';
import {FileUtil} from './fileutil';
import {FindOption} from './findoption';
import {FindSettings} from './findsettings';

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
            'encoding':
                (x: string, settings: FindSettings) => { settings.textFileEncoding = x; },
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
            'in-linesafterpattern':
                (x: string, settings: FindSettings) => { settings.addInLinesAfterPatterns(x); },
            'in-linesbeforepattern':
                (x: string, settings: FindSettings) => { settings.addInLinesBeforePatterns(x); },
            'linesafter':
                (x: string, settings: FindSettings) => { settings.linesAfter = parseInt(x); },
            'linesaftertopattern':
                (x: string, settings: FindSettings) => { settings.addLinesAfterToPatterns(x); },
            'linesafteruntilpattern':
                (x: string, settings: FindSettings) => { settings.addLinesAfterUntilPatterns(x); },
            'linesbefore':
                (x: string, settings: FindSettings) => { settings.linesBefore = parseInt(x); },
            'maxlinelength':
                (x: string, settings: FindSettings) => { settings.maxLineLength = parseInt(x); },
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
            'out-linesafterpattern':
                (x: string, settings: FindSettings) => { settings.addOutLinesAfterPatterns(x); },
            'out-linesbeforepattern':
                (x: string, settings: FindSettings) => { settings.addOutLinesBeforePatterns(x); },
            'findpattern':
                (x: string, settings: FindSettings) => { settings.addFindPatterns(x); },
            'settings-file':
                (x: string, settings: FindSettings) => { this.settingsFromFile(x, settings); }
        };

        this.boolFlagActionMap = {
            'allmatches':
                (b: boolean, settings: FindSettings) => { settings.firstMatch = !b; },
            'archivesonly':
                (b: boolean, settings: FindSettings) => { settings.setArchivesOnly(b); },
            'debug':
                (b: boolean, settings: FindSettings) => { settings.setDebug(b); },
            'excludehidden':
                (b: boolean, settings: FindSettings) => { settings.excludeHidden = b; },
            'firstmatch':
                (b: boolean, settings: FindSettings) => { settings.firstMatch = b; },
            'includehidden':
                (b: boolean, settings: FindSettings) => { settings.excludeHidden = !b; },
            'help':
                (b: boolean, settings: FindSettings) => { settings.printUsage = b; },
            'listdirs':
                (b: boolean, settings: FindSettings) => { settings.listDirs = b; },
            'listfiles':
                (b: boolean, settings: FindSettings) => { settings.listFiles = b; },
            'listlines':
                (b: boolean, settings: FindSettings) => { settings.listLines = b; },
            'multilineoption-REMOVE':
                (b: boolean, settings: FindSettings) => { settings.multilineFind = b; },
            'noprintmatches':
                (b: boolean, settings: FindSettings) => { settings.printResults = !b; },
            'norecursive':
                (b: boolean, settings: FindSettings) => { settings.recursive = !b; },
            'nofindarchives':
                (b: boolean, settings: FindSettings) => { settings.findArchives = !b; },
            'printmatches':
                (b: boolean, settings: FindSettings) => { settings.printResults = b; },
            'recursive':
                (b: boolean, settings: FindSettings) => { settings.recursive = b; },
            'findarchives':
                (b: boolean, settings: FindSettings) => { settings.findArchives = b; },
            'uniquelines':
                (b: boolean, settings: FindSettings) => { settings.uniqueLines = b; },
            'verbose':
                (b: boolean, settings: FindSettings) => { settings.verbose = b; },
            'version':
                (b: boolean, settings: FindSettings) => { settings.printVersion = b; }
        };

        this.setOptionsFromJsonFile();
    }

    private static optcmp(o1: FindOption, o2: FindOption) {
        const a: string = o1.sortarg;
        const b: string = o2.sortarg;
        return a.localeCompare(b);
    }

    // setOptionsFromJsonFile
    private setOptionsFromJsonFile(): void {
        const fs = require('fs');

        let json = '';
        if (fs.existsSync(FileUtil.expandPath(config.FINDOPTIONSJSONPATH))) {
            json = fs.readFileSync(FileUtil.expandPath(config.FINDOPTIONSJSONPATH)).toString();
        } else {
            throw new Error('File not found: ' + config.FINDOPTIONSJSONPATH);
        }

        const obj = JSON.parse(json);
        if (obj.hasOwnProperty('findoptions') && Array.isArray(obj['findoptions'])) {
            obj['findoptions'].forEach(so => {
                const longArg = so['long'];
                let shortArg = '';
                if (so.hasOwnProperty('short'))
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
        this.options.sort(FindOptions.optcmp);
    }

    private settingsFromFile(filepath: string, settings: FindSettings): Error | undefined {
        const fs = require('fs');
        if (fs.existsSync(filepath)) {
            const json: string = FileUtil.getFileContents(filepath, settings.textFileEncoding);
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
            if (obj.hasOwnProperty(k)) {
                if (this.argMap[k]) {
                    if (obj[k]) {
                        err = this.argActionMap[k](obj[k], settings);
                    } else {
                        err = new Error("Missing argument for option "+k);
                    }
                } else if (this.boolFlagActionMap[k]) {
                    this.boolFlagActionMap[k](obj[k], settings);
                } else if (k == 'startpath') {
                    settings.startPath = obj[k];
                } else {
                    err = new Error("Invalid option: "+k);
                }
            }
        }
        return err;
    }

    public settingsFromArgs(args: string[], cb: (err: Error | undefined, settings: FindSettings) => void) {
        let err: Error | undefined = undefined;
        const settings: FindSettings = new FindSettings();
        // default printResults to true since it's being run from cmd line
        settings.printResults = true;
        while(args && !err) {
            let arg: string = args.shift() || '';
            if (!arg) {
                break;
            }
            if (arg.charAt(0) === '-') {
                while (arg && arg.charAt(0) === '-') {
                    arg = arg.substring(1);
                }
                const longarg = this.argNameMap[arg];
                if (this.argMap[longarg]) {
                    if (args.length > 0) {
                        err = this.argActionMap[longarg](args.shift(), settings);
                    } else {
                        err = new Error("Missing argument for option " + arg);
                    }
                } else if (this.flagMap[longarg]) {
                    this.boolFlagActionMap[longarg](true, settings);
                } else {
                    err = new Error("Invalid option: " + arg);
                }
            } else {
                settings.startPath = arg;
            }
        }
        if (settings.debug) {
            settings.verbose = true;
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
        let usage: string = 'Usage:\n tsfind [options] -s <findpattern>' +
            ' <startpath>\n\nOptions:\n';
        const optStrings: string[] = [];
        const optDescs: string[] = [];
        let longest = 0;
        this.options.forEach((opt: FindOption) => {
            let optString = ' ';
            if (opt.shortarg)
                optString += '-' + opt.shortarg + ',';
            optString += '--' + opt.longarg;
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
