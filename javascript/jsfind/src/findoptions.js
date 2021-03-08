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
            'maxlastmod':
                (x, settings) => {
                    /* TODO: add maxlastmod as date/time */
                    x != null; settings != null;
                },
            'maxsize':
                (x, settings) => {
                    /* TODO: add maxsize as int */
                    x != null; settings != null;
                },
            'minlastmod':
                (x, settings) => {
                    /* TODO: add minlastmod as date/time */
                    x != null; settings != null;
                },
            'minsize':
                (x, settings) => {
                    /* TODO: add minsize as int */
                    x != null; settings != null;
                },
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
            'path':
                (x, settings) => { settings.paths.push(x); },
            'settings-file':
                (x, settings) => { this.settingsFromFile(x, settings); }

        };
        this.boolFlagActionMap = {
            'archivesonly':
                (b, settings) => { settings.setArchivesOnly(b); },
            'colorize':
                (b, settings) => { settings.colorize = b; },
            'debug':
                (b, settings) => { settings.setDebug(b); },
            'excludearchives':
                (b, settings) => { settings.includeArchives = !b; },
            'excludehidden':
                (b, settings) => { settings.excludeHidden = b; },
            'includearchives':
                (b, settings) => { settings.includeArchives = b; },
            'includehidden':
                (b, settings) => { settings.excludeHidden = !b; },
            'help':
                (b, settings) => { settings.printUsage = b; },
            'listdirs':
                (b, settings) => { settings.listDirs = b; },
            'listfiles':
                (b, settings) => { settings.listFiles = b; },
            'nocolorize':
                (b, settings) => { settings.colorize = !b; },
            'norecursive':
                (b, settings) => { settings.recursive = !b; },
            'recursive':
                (b, settings) => { settings.recursive = b; },
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
                throw new FindError(`File not found: ${config.FINDOPTIONSJSONPATH}`);
            }

            let obj = JSON.parse(json);
            if (Object.prototype.hasOwnProperty.call(obj, 'findoptions') && Array.isArray(obj['findoptions'])) {
                obj['findoptions'].forEach(so => {
                    let longArg = so['long'];
                    let shortArg = '';
                    if (Object.prototype.hasOwnProperty.call(so, 'short'))
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
            } else throw new FindError(`Invalid findoptions file: ${config.FINDOPTIONSJSONPATH}`);
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
            throw new FindError('Settings file not found');
        }
    }

    settingsFromJson(json, settings) {
        // TODO: should err be thrown as in settingsFromFile or returned a in settingsFromArgs?
        let err = null;
        let obj = JSON.parse(json);
        for (let k in obj) {
            if (err) break;
            if (Object.prototype.hasOwnProperty.call(obj, k)) {
                let longKey = this.argNameMap[k];
                if (this.argMap[k]) {
                    if (obj[k]) {
                        this.argMap[k].func(obj[k], settings);
                    } else {
                        err = new Error(`Missing argument for option ${k}`);
                    }
                } else if (this.boolFlagActionMap[longKey]) {
                    this.boolFlagActionMap[longKey](obj[k], settings);
                } else if (k === 'path') {
                    settings.paths.push(obj[k]);
                } else {
                    err = new FindError(`Invalid option: ${k}`);
                }
            }
        }
        return err;
    }

    settingsFromArgs(args, cb) {
        let err = null;
        let settings = new FindSettings();
        // default listFiles to true since running as cli
        settings.listFiles = true;

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
                        err = new Error(`Missing argument for option ${arg}`);
                    }
                } else if (this.flagMap[arg]) {
                    this.flagMap[arg].func(true, settings);
                } else {
                    err = new Error(`Invalid option: ${arg}`);
                }
            } else {
                settings.paths.push(arg);
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
        let usage = 'Usage:\n jsfind [options] <path> [<path> ...]\n\n';
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
