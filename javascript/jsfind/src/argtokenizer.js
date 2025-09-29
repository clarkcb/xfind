/*
 * argtokenizer.js
 *
 * tokenizes command line arguments
 */

const {ArgToken} = require("./argtoken");
const {ArgTokenType} = require("./argtokentype");
const { FindError } = require('./finderror')
const { FileUtil } = require('./fileutil')
const fs = require('fs')

class ArgTokenizer {
    boolMap
    strMap
    intMap

    constructor(boolMap, strMap, intMap) {
        this.boolMap = boolMap;
        this.strMap = strMap;
        this.intMap = intMap;
    }

    tokenizeArgs(args) {
        let err = null;
        let argTokens = [];

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
                            argNames.push(arg);
                        } else {
                            err = new Error(`Invalid option: ${arg}`);
                            break;
                        }
                    } else {
                        arg = arg.substring(1);
                        for (const c of arg) {
                            if (this.boolMap[c]) {
                                argNames.push(this.boolMap[c]);
                            } else if (this.strMap[c]) {
                                argNames.push(this.strMap[c]);
                            } else if (this.intMap[c]) {
                                argNames.push(this.intMap[c]);
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
                    if (this.boolMap[argName]) {
                        argTokens.push(new ArgToken(this.boolMap[argName], ArgTokenType.Bool, true));
                    } else if (this.strMap[argName] || this.intMap[argName] || argName === 'settings-file') {
                        if (args.length > 0) {
                            const argValue = args.shift();
                            if (this.strMap[argName]) {
                                argTokens.push(new ArgToken(this.strMap[argName], ArgTokenType.Str, argValue));
                            } else if (this.intMap[argName]) {
                                argTokens.push(new ArgToken(this.intMap[argName], ArgTokenType.Int, parseInt(argValue, 10)));
                            } else {
                                argTokens.push(new ArgToken('settings-file', ArgTokenType.Str, argValue));
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
                argTokens.push(new ArgToken('path', ArgTokenType.Str, arg));
            }
        }
        return { err, argTokens };
    }

    tokenizeArgsObj(argsObj) {
        let err = null;
        let argTokens = [];
        // keys are sorted so that output is consistent across all versions
        const argNames = Object.keys(argsObj).sort();
        for (const argName of argNames) {
            if (err) break;
            if (Object.prototype.hasOwnProperty.call(argsObj, argName)) {
                if (argsObj[argName] !== undefined && argsObj[argName] !== null) {
                    // let longArg = this.argNameMap[argName];
                    if (this.boolMap[argName]) {
                        if (typeof argsObj[argName] === 'boolean') {
                            argTokens.push(new ArgToken(this.boolMap[argName], ArgTokenType.Bool, argsObj[argName]));
                        } else {
                            err = new FindError(`Invalid value for option: ${argName}`);
                        }
                    } else if (this.strMap[argName]) {
                        if (typeof argsObj[argName] === 'string') {
                            argTokens.push(new ArgToken(this.strMap[argName], ArgTokenType.Str, argsObj[argName]));
                        } else if (typeof argsObj[argName] === 'object' && Array.isArray(argsObj[argName])) {
                            argsObj[argName].forEach(s => {
                                if (typeof s === 'string') {
                                    argTokens.push(new ArgToken(this.strMap[argName], ArgTokenType.Str, s));
                                } else {
                                    err = new FindError(`Invalid value for option: ${argName}`);
                                }
                            });
                        } else {
                            err = new FindError(`Invalid value for option: ${argName}`);
                        }
                    } else if (this.intMap[argName]) {
                        if (typeof argsObj[argName] === 'number') {
                            argTokens.push(new ArgToken(this.intMap[argName], ArgTokenType.Int, argsObj[argName]));
                        } else {
                            err = new FindError(`Invalid value for option: ${argName}`);
                        }
                    } else if (argName === 'settings-file') {
                        argTokens.push(new ArgToken('settings-file', ArgTokenType.Str, argsObj[argName]));
                    } else {
                        err = new FindError(`Invalid option: ${argName}`);
                    }
                } else {
                    err = new FindError(`Missing value for option ${argName}`);
                }
            }
        }
        return { err, argTokens };
    }

    tokenizeJson(json) {
        let err = null;
        let argTokens = [];
        try {
            const obj = JSON.parse(json);
            return this.tokenizeArgsObj(obj);
        } catch (e) {
            if (e instanceof SyntaxError) {
                err = new FindError(`Invalid JSON in settings: ${e.message}`);
            } else {
                err = new FindError(`Error parsing settings JSON: ${e.message}`);
            }
            return { err, argTokens };
        }
    }

    tokenizeFile(filePath) {
        let err = null;
        let argTokens = [];
        const expandedPath = FileUtil.expandPath(filePath);
        if (fs.existsSync(expandedPath)) {
            if (expandedPath.endsWith('.json')) {
                let json = FileUtil.getFileContentsSync(expandedPath, 'utf-8');
                return this.tokenizeJson(json);
            }
            err = new FindError(`Invalid settings file (must be JSON): ${filePath}`);
            return { err, argTokens };
        } else {
            err = new FindError(`Settings file not found: ${filePath}`);
            return { err, argTokens };
        }
    }
}

exports.ArgTokenizer = ArgTokenizer;
