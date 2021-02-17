/*
 * findoptions.js
 *
 * defines the set of find options and provides functionality to define find settings from them
 */

const config = require('./config');
const {expandPath} = require('./fileutil');
const {FindError} = require('./finderror');
const {FindOption} = require('./findoption');
const {FindSettings} = require('./findsettings');

class FindOptions {
    'use strict'

    constructor() {
        this.argNameMap = {};
        this.argMap = {};
        this.flagMap = {};
        this.argActionMap = {
            'encoding':
                (x, settings) => { settings.textFileEncoding = x; },
            'in-archiveext':
                (x, settings) => { settings.addInArchiveExtensions(x); },
            'in-archivefilepattern':
                (x, settings) => { settings.addInArchiveFilePatterns(x); },
            'in-dirpattern':
                (x, settings) => { settings.addInDirPatterns(x); },
            'in-ext':
                (x, settings) => { settings.addInExtensions(x); },
            'in-filepattern':
                (x, settings) => { settings.addInFilePatterns(x); },
            'in-filetype':
                (x, settings) => { settings.addInFileTypes(x); },
            'in-linesafterpattern':
                (x, settings) => { settings.addInLinesAfterPatterns(x); },
            'in-linesbeforepattern':
                (x, settings) => { settings.addInLinesBeforePatterns(x); },
            'linesafter':
                (x, settings) => { settings.linesAfter = parseInt(x); },
            'linesaftertopattern':
                (x, settings) => { settings.addLinesAfterToPatterns(x); },
            'linesafteruntilpattern':
                (x, settings) => { settings.addLinesAfterUntilPatterns(x); },
            'linesbefore':
                (x, settings) => { settings.linesBefore = parseInt(x); },
            'maxlinelength':
                (x, settings) => { settings.maxLineLength = parseInt(x); },
            'out-dirpattern':
                (x, settings) => { settings.addOutDirPatterns(x); },
            'out-archiveext':
                (x, settings) => { settings.addOutArchiveExtensions(x); },
            'out-archivefilepattern':
                (x, settings) => { settings.addOutArchiveFilePatterns(x); },
            'out-ext':
                (x, settings) => { settings.addOutExtensions(x); },
            'out-filepattern':
                (x, settings) => { settings.addOutFilePatterns(x); },
            'out-filetype':
                (x, settings) => { settings.addOutFileTypes(x); },
            'out-linesafterpattern':
                (x, settings) => { settings.addOutLinesAfterPatterns(x); },
            'out-linesbeforepattern':
                (x, settings) => { settings.addOutLinesBeforePatterns(x); },
            'findpattern':
                (x, settings) => { settings.addFindPatterns(x); },
            'settings-file':
                (x, settings) => { return settingsFromFile(x, settings); }

        };
        this.boolFlagActionMap = {
            'allmatches':
                (b, settings) => { settings.firstMatch = !b; },
            'archivesonly':
                (b, settings) => { settings.setArchivesOnly(b); },
            'colorize':
                (b, settings) => { settings.colorize = b; },
            'debug':
                (b, settings) => { settings.setDebug(b); },
            'excludehidden':
                (b, settings) => { settings.excludeHidden = b; },
            'firstmatch':
                (b, settings) => { settings.firstMatch = b; },
            'includehidden':
                (b, settings) => { settings.excludeHidden = !b; },
            'help':
                (b, settings) => { settings.printUsage = b; },
            'listdirs':
                (b, settings) => { settings.listDirs = b; },
            'listfiles':
                (b, settings) => { settings.listFiles = b; },
            'listlines':
                (b, settings) => { settings.listLines = b; },
            'multilineoption-REMOVE':
                (b, settings) => { settings.multilineFind = b; },
            'nocolorize':
                (b, settings) => { settings.colorize = !b; },
            'noprintmatches':
                (b, settings) => { settings.printResults = !b; },
            'norecursive':
                (b, settings) => { settings.recursive = !b; },
            'nofindarchives':
                (b, settings) => { settings.findArchives = !b; },
            'printmatches':
                (b, settings) => { settings.printResults = b; },
            'recursive':
                (b, settings) => { settings.recursive = b; },
            'findarchives':
                (b, settings) => { settings.findArchives = b; },
            'uniquelines':
                (b, settings) => { settings.uniqueLines = b; },
            'verbose':
                (b, settings) => { settings.verbose = b; },
            'version':
                (b, settings) => { settings.printVersion = b; }
        };
        // the list of FindOption objects (populated from JSON)
        this.options = [];
        (() => {
            const fs = require('fs');

            let json = '';
            if (fs.existsSync(expandPath(config.FINDOPTIONSJSONPATH))) {
                json = fs.readFileSync(expandPath(config.FINDOPTIONSJSONPATH)).toString();
            } else {
                throw new FindError('File not found: ' + config.FINDOPTIONSJSONPATH);
            }

            let obj = JSON.parse(json);
            if (obj.hasOwnProperty('findoptions') && Array.isArray(obj['findoptions'])) {
                obj['findoptions'].forEach(so => {
                    let longArg = so['long'];
                    let shortArg = '';
                    if (so.hasOwnProperty('short'))
                        shortArg = so['short'];
                    let desc = so['desc'];
                    let func = null;
                    this.argNameMap[longArg] = longArg;
                    if (shortArg) this.argNameMap[shortArg] = longArg;
                    if (this.argActionMap[longArg]) func = this.argActionMap[longArg];
                    else if (this.boolFlagActionMap[longArg]) func = this.boolFlagActionMap[longArg];
                    else throw new FindError("Unknown option: " + longArg);
                    const option = new FindOption(shortArg, longArg, desc, func);
                    this.options.push(option);
                    if (this.argActionMap[longArg]) {
                        this.argMap[longArg] = option;
                        if (shortArg) this.argMap[shortArg] = option;
                    } else if (this.boolFlagActionMap[longArg]) {
                        this.flagMap[longArg] = option;
                        if (shortArg) this.flagMap[shortArg] = option;
                    }
                });
            } else throw new FindError("Invalid findoptions file: " + config.FINDOPTIONSJSONPATH);
            this.options.sort(this.optcmp);
        })();

    }

    optcmp(o1, o2) {
        const a = o1.sortarg;
        const b = o2.sortarg;
        return a.localeCompare(b);
    }

    settingsFromFile(filepath, settings) {
        const fs = require('fs');
        if (fs.existsSync(filepath)) {
            let json = fs.readFileSync(filepath).toString();
            return this.settingsFromJson(json, settings);
        } else {
            return new FindError('Settings file not found');
        }
    }

    settingsFromJson(json, settings) {
        let err = null;
        let obj = JSON.parse(json);
        for (let k in obj) {
            if (err) break;
            if (obj.hasOwnProperty(k)) {
                let longKey = this.argNameMap[k];
                if (this.argMap[k]) {
                    if (obj[k]) {
                        this.argMap[k].func(obj[k], settings);
                    } else {
                        err = new Error("Missing argument for option " + k);
                    }
                } else if (this.boolFlagActionMap[longKey]) {
                    this.boolFlagActionMap[longKey](obj[k], settings);
                } else if (k === 'startpath') {
                    settings.startPath = obj[k];
                } else {
                    err = new FindError("Invalid option: " + k);
                }
            }
        }
        return err;
    }

    settingsFromArgs(args, cb) {
        let err = null;
        let settings = new FindSettings();

        // default printResults to true since it's being run from cmd line
        settings.printResults = true;
        while(args && !err) {
            let arg = args.shift();
            if (!arg) {
                break;
            }
            if (arg.charAt(0) === '-') {
                while (arg && arg.charAt(0) === '-') {
                    arg = arg.substring(1);
                }
                if (this.argMap[arg]) {
                    if (args.length > 0) {
                        err = this.argMap[arg].func(args.shift(), settings);
                    } else {
                        err = new Error("Missing argument for option " + arg);
                    }
                } else if (this.flagMap[arg]) {
                    this.flagMap[arg].func(true, settings);
                } else {
                    err = new Error("Invalid option: " + arg);
                }
            } else {
                settings.startPath = arg;
            }
        }
        cb(err, settings);
    }

    usage() {
        this.usageWithCode(0);
    }

    usageWithCode(exitCode) {
        console.log(this.getUsageString());
        process.exit(exitCode);
    }

    getUsageString() {
        let usage = 'Usage:\n jsfind [options] -s <findpattern> <startpath>\n\n';
        usage += 'Options:\n';
        let optStrings = [];
        let optDescs = [];
        let longest = 0;
        this.options.forEach(opt => {
            let optString = ' ';
            if (opt.shortarg)
                optString += '-' + opt.shortarg + ',';
            optString += '--' + opt.longarg;
            if (optString.length > longest)
                longest = optString.length;
            optStrings.push(optString);
            optDescs.push(opt.desc);
        });
        for (let i=0; i < optStrings.length; i++) {
            let os = optStrings[i];
            while (os.length < longest)
                os += ' ';
            usage += os + '  ' + optDescs[i] + '\n';
        }
        return usage;
    }
}

exports.FindOptions = FindOptions;
