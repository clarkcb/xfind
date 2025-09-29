/*
 * findoptions.js
 *
 * defines the set of find options and provides functionality to define find settings from them
 */

const config = require('./config');
const {ArgTokenizer} = require('./argtokenizer');
const {ArgTokenType} = require('./argtokentype')
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
        this.boolMap = {};
        this.strMap = {'path': 'path'};
        this.intMap = {};

        // populate options from JSON file
        (() => {
            let json = FileUtil.getFileContentsSync(config.FIND_OPTIONS_JSON_PATH, 'utf-8');
            let obj = JSON.parse(json);
            if (Object.prototype.hasOwnProperty.call(obj, 'findoptions') && Array.isArray(obj.findoptions)) {
                obj.findoptions.forEach(fo => {
                    let longArg = fo.long;
                    this.argNameMap[longArg] = longArg;
                    if (this.boolActionMap[longArg]) {
                        this.boolMap[longArg] = longArg;
                    } else if (this.stringActionMap[longArg]) {
                        this.strMap[longArg] = longArg;
                    } else if (this.intActionMap[longArg]) {
                        this.intMap[longArg] = longArg;
                    }
                    let shortArg = '';
                    if (Object.prototype.hasOwnProperty.call(fo, 'short')) {
                        shortArg = fo.short;
                        this.argNameMap[shortArg] = longArg;
                        if (this.boolActionMap[longArg]) {
                            this.boolMap[shortArg] = longArg;
                        } else if (this.stringActionMap[longArg]) {
                            this.strMap[shortArg] = longArg;
                        } else if (this.intActionMap[longArg]) {
                            this.intMap[shortArg] = longArg;
                        }
                    }
                    let desc = fo.desc;
                    const option = new FindOption(shortArg, longArg, desc);
                    this.options.push(option);
                });
            } else throw new FindError(`Invalid findoptions file: ${config.FIND_OPTIONS_JSON_PATH}`);
            this.options.sort(this.optCmp);
            this.argTokenizer = new ArgTokenizer(this.boolMap, this.strMap, this.intMap);
        })();
    }

    optCmp(o1, o2) {
        const a = o1.sortArg;
        const b = o2.sortArg;
        return a.localeCompare(b);
    }

    updateSettingsFromArgTokens(settings, argTokens) {
        let err = null;
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

    updateSettingsFromJson(settings, json) {
        let { err, argTokens } = this.argTokenizer.tokenizeJson(json);
        if (!err) {
            err = this.updateSettingsFromArgTokens(settings, argTokens);
        }
        return err;
    }

    updateSettingsFromFile(settings, filePath) {
        let { err, argTokens } = this.argTokenizer.tokenizeFile(filePath);
        if (!err) {
            err = this.updateSettingsFromArgTokens(settings, argTokens);
        }
        return err;
    }

    updateSettingsFromArgs(settings, args) {
        let { err, argTokens } = this.argTokenizer.tokenizeArgs(args);
        if (!err) {
            err = this.updateSettingsFromArgTokens(settings, argTokens);
        }
        return err;
    }

    settingsFromArgs(args, cb) {
        let settings = new FindSettings();
        // default printFiles to true since running as cli
        settings.printFiles = true;
        let err = this.updateSettingsFromArgs(settings, args);
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
