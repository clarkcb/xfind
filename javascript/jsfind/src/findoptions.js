/*
 * findoptions.js
 *
 * defines the set of find options and provides functionality to define find settings from them
 */

const fs = require('fs');

const config = require('./config');
const {FileUtil} = require('./fileutil');
const {FindError} = require('./finderror');
const {FindOption} = require('./findoption');
const {FindSettings} = require('./findsettings');
const {nameToSortBy} = require("./sortby");

class FindOptions {
    constructor() {
        // path is separate because it is not included as an option in findoptions.json
        this.argNameMap = {'path' : 'path'};
        this.boolActionMap = {
            'archivesonly':
              (b, settings) => { settings.archivesOnly = b; },
            'debug':
              (b, settings) => { settings.debug = b; },
            'excludearchives':
              (b, settings) => { settings.includeArchives = !b; },
            'excludehidden':
              (b, settings) => { settings.includeHidden = !b; },
            'followsymlinks':
              (b, settings) => { settings.followSymlinks = b; },
            'help':
              (b, settings) => { settings.printUsage = b; },
            'includearchives':
              (b, settings) => { settings.includeArchives = b; },
            'includehidden':
              (b, settings) => { settings.includeHidden = b; },
            'nofollowsymlinks':
              (b, settings) => { settings.followSymlinks = !b; },
            'noprintdirs':
              (b, settings) => { settings.printDirs = !b; },
            'noprintfiles':
              (b, settings) => { settings.printFiles = !b; },
            'norecursive':
              (b, settings) => { settings.recursive = !b; },
            'printdirs':
              (b, settings) => { settings.printDirs = b; },
            'printfiles':
              (b, settings) => { settings.printFiles = b; },
            'recursive':
              (b, settings) => { settings.recursive = b; },
            'sort-ascending':
              (b, settings) => { settings.sortDescending = !b; },
            'sort-caseinsensitive':
              (b, settings) => { settings.sortCaseInsensitive = b; },
            'sort-casesensitive':
              (b, settings) => { settings.sortCaseInsensitive = !b; },
            'sort-descending':
              (b, settings) => { settings.sortDescending = b; },
            'verbose':
              (b, settings) => { settings.verbose = b; },
            'version':
              (b, settings) => { settings.printVersion = b; }
        };
        this.stringActionMap = {
            'in-archiveext':
              (s, settings) => { settings.addInArchiveExtensions(s); },
            'in-archivefilepattern':
              (s, settings) => { settings.addInArchiveFilePatterns(s); },
            'in-dirpattern':
              (s, settings) => { settings.addInDirPatterns(s); },
            'in-ext':
              (s, settings) => { settings.addInExtensions(s); },
            'in-filepattern':
              (s, settings) => { settings.addInFilePatterns(s); },
            'in-filetype':
              (s, settings) => { settings.addInFileTypes(s); },
            'maxlastmod':
              (s, settings) => { settings.maxLastModFromString(s); },
            'minlastmod':
              (s, settings) => { settings.minLastModFromString(s); },
            'out-dirpattern':
              (s, settings) => { settings.addOutDirPatterns(s); },
            'out-archiveext':
              (s, settings) => { settings.addOutArchiveExtensions(s); },
            'out-archivefilepattern':
              (s, settings) => { settings.addOutArchiveFilePatterns(s); },
            'out-ext':
              (s, settings) => { settings.addOutExtensions(s); },
            'out-filepattern':
              (s, settings) => { settings.addOutFilePatterns(s); },
            'out-filetype':
              (s, settings) => { settings.addOutFileTypes(s); },
            'path':
              (s, settings) => { settings.paths.push(s); },
            'sort-by':
              (s, settings) => { settings.sortBy = nameToSortBy(s); }
        };
        this.intActionMap = {
            'maxdepth':
              (i, settings) => { settings.maxDepth = i; },
            'maxsize':
              (i, settings) => { settings.maxSize = i; },
            'mindepth':
              (i, settings) => { settings.minDepth = i; },
            'minsize':
              (i, settings) => { settings.minSize = i; },
        };

        // the list of FindOption objects (populated from JSON)
        this.options = [];
        (() => {
            let json = FileUtil.getFileContentsSync(config.FIND_OPTIONS_JSON_PATH, 'utf-8');
            let obj = JSON.parse(json);
            if (Object.prototype.hasOwnProperty.call(obj, 'findoptions') && Array.isArray(obj.findoptions)) {
                obj.findoptions.forEach(fo => {
                    let longArg = fo.long;
                    this.argNameMap[longArg] = longArg;
                    let shortArg = '';
                    if (Object.prototype.hasOwnProperty.call(fo, 'short')) {
                        shortArg = fo.short;
                        this.argNameMap[shortArg] = longArg;
                    }
                    let desc = fo.desc;
                    const option = new FindOption(shortArg, longArg, desc);
                    this.options.push(option);
                });
            } else throw new FindError(`Invalid findoptions file: ${config.FIND_OPTIONS_JSON_PATH}`);
            this.options.sort(this.optCmp);
        })();
    }

    optCmp(o1, o2) {
        const a = o1.sortArg;
        const b = o2.sortArg;
        return a.localeCompare(b);
    }

    updateSettingsFromJson(json, settings) {
        let err = null;
        const obj = JSON.parse(json);
        // keys are sorted so that output is consistent across all versions
        const keys = Object.keys(obj).sort();
        const invalidKeys = keys.filter(k => !Object.prototype.hasOwnProperty.call(this.argNameMap, k));
        if (invalidKeys.length > 0) {
            return new FindError(`Invalid option: ${invalidKeys[0]}`);
        }
        for (const k of keys) {
            if (err) break;
            if (Object.prototype.hasOwnProperty.call(obj, k)) {
                if (obj[k] !== undefined && obj[k] !== null) {
                    // let longArg = k === 'path' ? 'path' : this.argNameMap[k];
                    let longArg = this.argNameMap[k];
                    if (this.boolActionMap[longArg]) {
                        if (typeof obj[k] === 'boolean') {
                            this.boolActionMap[longArg](obj[k], settings);
                        } else {
                            err = new FindError(`Invalid value for option: ${k}`);
                        }
                    } else if (this.stringActionMap[longArg]) {
                        if (typeof obj[k] === 'string') {
                            this.stringActionMap[longArg](obj[k], settings);
                        } else if (typeof obj[k] === 'object' && Array.isArray(obj[k])) {
                            obj[k].forEach(s => {
                                if (typeof s === 'string') {
                                    this.stringActionMap[longArg](s, settings);
                                } else {
                                    err = new FindError(`Invalid value for option: ${k}`);
                                }
                            });
                        } else {
                            err = new FindError(`Invalid value for option: ${k}`);
                        }
                    } else if (this.intActionMap[longArg]) {
                        if (typeof obj[k] === 'number') {
                            this.intActionMap[longArg](obj[k], settings);
                        } else {
                            err = new FindError(`Invalid value for option: ${k}`);
                        }
                    } else {
                        err = new FindError(`Invalid option: ${k}`);
                    }
                } else {
                    err = new FindError(`Missing value for option ${k}`);
                }
            }
        }
        return err;
    }

    updateSettingsFromFile(filePath, settings) {
        const expandedPath = FileUtil.expandPath(filePath);
        if (fs.existsSync(expandedPath)) {
            if (expandedPath.endsWith('.json')) {
                let json = FileUtil.getFileContentsSync(expandedPath, 'utf-8');
                return this.updateSettingsFromJson(json, settings);
            }
            return new FindError(`Invalid settings file (must be JSON): ${filePath}`);
        } else {
            return new FindError(`Settings file not found: ${filePath}`);
        }
    }

    settingsFromArgs(args, cb) {
        let err = null;
        let settings = new FindSettings();
        // default printFiles to true since running as cli
        settings.printFiles = true;

        while(args && !err) {
            let arg = args.shift();
            if (!arg) {
                break;
            }
            if (arg.charAt(0) === '-') {
                while (arg && arg.charAt(0) === '-') {
                    arg = arg.substring(1);
                }
                let longArg = this.argNameMap[arg];
                if (this.boolActionMap[longArg]) {
                    this.boolActionMap[longArg](true, settings);
                } else if (this.stringActionMap[longArg] || this.intActionMap[longArg] || longArg === 'settings-file') {
                    if (args.length > 0) {
                        if (this.stringActionMap[longArg]) {
                            err = this.stringActionMap[longArg](args.shift(), settings);
                        } else if (this.intActionMap[longArg]) {
                            err = this.intActionMap[longArg](parseInt(args.shift(), 10), settings);
                        } else {
                            err = this.updateSettingsFromFile(args.shift(), settings);
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

    getUsageString() {
        let usage = 'Usage:\n jsfind [options] <path> [<path> ...]\n\n';
        usage += 'Options:\n';
        let optStrings = [];
        let optDescs = [];
        let longest = 0;
        this.options.forEach(opt => {
            let optString = ' ';
            if (opt.shortArg)
                optString += '-' + opt.shortArg + ',';
            optString += '--' + opt.longArg;
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

    usageWithCode(exitCode) {
        console.log(this.getUsageString());
        process.exit(exitCode);
    }

    usage() {
        this.usageWithCode(0);
    }
}

exports.FindOptions = FindOptions;
