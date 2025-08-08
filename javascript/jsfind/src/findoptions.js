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
            'colorize':
              (b, settings) => { settings.colorize = b; },
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
            'nocolorize':
              (b, settings) => { settings.colorize = !b; },
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

    updateSettingsFromArgMap(settings, argMap) {
        let err = null;
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
                    let longArg = this.argNameMap[argName];
                    if (this.boolActionMap[longArg]) {
                        if (typeof argMap[argName] === 'boolean') {
                            this.boolActionMap[longArg](argMap[argName], settings);
                        } else {
                            err = new FindError(`Invalid value for option: ${argName}`);
                        }
                    } else if (this.stringActionMap[longArg]) {
                        if (typeof argMap[argName] === 'string') {
                            this.stringActionMap[longArg](argMap[argName], settings);
                        } else if (typeof argMap[argName] === 'object' && Array.isArray(argMap[argName])) {
                            argMap[argName].forEach(s => {
                                if (typeof s === 'string') {
                                    this.stringActionMap[longArg](s, settings);
                                } else {
                                    err = new FindError(`Invalid value for option: ${argName}`);
                                }
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

    updateSettingsFromJson(settings, json) {
        try {
            const obj = JSON.parse(json);
            return this.updateSettingsFromArgMap(settings, obj);
        } catch (e) {
            if (e instanceof SyntaxError) {
                return new FindError(`Invalid JSON in settings: ${e.message}`);
            } else {
                return new FindError(`Error parsing settings JSON: ${e.message}`);
            }
        }
    }

    updateSettingsFromFile(settings, filePath) {
        const expandedPath = FileUtil.expandPath(filePath);
        if (fs.existsSync(expandedPath)) {
            if (expandedPath.endsWith('.json')) {
                let json = FileUtil.getFileContentsSync(expandedPath, 'utf-8');
                return this.updateSettingsFromJson(settings, json);
            }
            return new FindError(`Invalid settings file (must be JSON): ${filePath}`);
        } else {
            return new FindError(`Settings file not found: ${filePath}`);
        }
    }

    argMapFromArgs(args) {
        let err = null;
        let argMap = {};

        while(args && !err) {
            let arg = args.shift();
            if (!arg) {
                break;
            }
            if (arg.charAt(0) === '-') {
                let argNames = [];
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
                            const argValue = args.shift();
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

    settingsFromArgs(args, cb) {
        let settings = new FindSettings();
        // default printFiles to true since running as cli
        settings.printFiles = true;
        let { err, argMap } = this.argMapFromArgs(args);
        if (!err) {
            err = this.updateSettingsFromArgMap(settings, argMap);
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
