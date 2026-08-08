/*
 * argparser.ts
 *
 * parses arg list to map
 */

'use strict';

import { ArgToken } from './argtoken';
import { ArgTokenType } from './argtokentype';
import { FindError } from './finderror';
import fs from 'fs';
import { FileUtil } from './fileutil';
import { Option } from './findoption';
import { StringUtil } from './stringutil';

const numModPattern = new RegExp('^\\d+[ckmgtp]$', 'i');

export interface ArgTokenizerResult {
  err: Error | null | undefined;
  argTokens: ArgToken[];
}

export class ArgTokenizer {
  boolMap: Map<string, string>;
  strMap: Map<string, string>;
  intMap: Map<string, string>;

  constructor(options: Option[]) {
    this.boolMap = new Map<string, string>();
    this.strMap = new Map<string, string>();
    this.intMap = new Map<string, string>();
    options.forEach((opt) => {
      if (opt.argType === ArgTokenType.Bool) {
        this.boolMap.set(opt.longArg, opt.longArg);
        if (opt.shortArg) {
          this.boolMap.set(opt.shortArg, opt.longArg);
        }
      } else if (opt.argType === ArgTokenType.Str) {
        this.strMap.set(opt.longArg, opt.longArg);
        if (opt.shortArg) {
          this.strMap.set(opt.shortArg, opt.longArg);
        }
      } else if (opt.argType === ArgTokenType.Int) {
        this.intMap.set(opt.longArg, opt.longArg);
        if (opt.shortArg) {
          this.intMap.set(opt.shortArg, opt.longArg);
        }
      }
    });
  }

  public tokenizeSizeArg(
    argName: string,
    argValue: string,
  ): { err: null | Error; argToken: ArgToken | null } {
    let i = 0;
    let err: null | Error = null;
    let argToken: null | ArgToken = null;
    if (argValue.search(numModPattern) > -1) {
      let num = parseInt(argValue.substring(0, argValue.length - 1), 10);
      let modifier = argValue.substring(argValue.length - 1).toLowerCase();
      if (modifier === 'k') {
        i = num * 1024;
      } else if (modifier === 'm') {
        i = num * 1024 * 1024;
      } else if (modifier === 'g') {
        i = num * 1024 * 1024 * 1024;
      } else if (modifier === 't') {
        i = num * 1024 * 1024 * 1024 * 1024;
      } else if (modifier === 'p') {
        i = num * 1024 * 1024 * 1024 * 1024 * 1024;
      }
      argToken = new ArgToken(this.intMap.get(argName)!, ArgTokenType.Int, i);
    } else {
      i = StringUtil.stringToInt(argValue);
      if (isNaN(i)) {
        err = new Error(`Invalid value for option ${argName}: ${argValue}`);
      } else {
        argToken = new ArgToken(this.intMap.get(argName)!, ArgTokenType.Int, i);
      }
    }
    return { err, argToken };
  }

  tokenizeIntArg(
    argName: string,
    argValue: string,
  ): { err: null | Error; argToken: ArgToken | null } {
    if (['maxsize', 'minsize'].includes(argName)) {
      return this.tokenizeSizeArg(argName, argValue);
    }
    let i = StringUtil.stringToInt(argValue);
    let err: null | Error = null;
    let argToken: null | ArgToken = null;
    if (isNaN(i)) {
      err = new Error(`Invalid value for option ${argName}: ${argValue}`);
    } else {
      argToken = new ArgToken(this.intMap.get(argName)!, ArgTokenType.Int, i);
    }
    return { err, argToken };
  }

  public tokenizeArgs(args: string[]): ArgTokenizerResult {
    let err: Error | null | undefined = undefined;
    const argTokens: ArgToken[] = [];

    while (args && !err) {
      let arg = args.shift();
      if (!arg) {
        break;
      }
      if (arg.charAt(0) === '-') {
        const argNames: string[] = [];
        if (arg.length > 1) {
          if (arg.charAt(1) === '-') {
            if (arg.length > 2) {
              arg = arg.substring(2);
              if (arg.indexOf('=') > -1) {
                const parts = arg.split('=');
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
              if (this.boolMap.has(c)) {
                argNames.push(this.boolMap.get(c)!);
              } else if (this.strMap.has(c)) {
                argNames.push(this.strMap.get(c)!);
              } else if (this.intMap.has(c)) {
                argNames.push(this.intMap.get(c)!);
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
          if (this.boolMap.has(argName)) {
            argTokens.push(new ArgToken(this.boolMap.get(argName)!, ArgTokenType.Bool, true));
          } else if (
            this.strMap.has(argName) ||
            this.intMap.has(argName) ||
            argName === 'settings-file'
          ) {
            if (args.length > 0) {
              const argValue = args.shift()!;
              if (this.strMap.has(argName)) {
                argTokens.push(new ArgToken(this.strMap.get(argName)!, ArgTokenType.Str, argValue));
              } else if (this.intMap.has(argName)) {
                let argToken: ArgToken | null = null;
                ({ err, argToken } = this.tokenizeIntArg(argName, argValue));
                if (err) break;
                argTokens.push(argToken!);
              } else {
                argTokens.push(new ArgToken('settings-file', ArgTokenType.Str, argValue));
              }
            } else {
              err = new Error(`Missing value for option ${arg}`);
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

  public tokenizeArgsObj(argsObj: { [argName: string]: any }): ArgTokenizerResult {
    let err: Error | undefined = undefined;
    const argTokens: ArgToken[] = [];
    // keys are sorted so that output is consistent across all versions
    const argNames = Object.keys(argsObj).sort();
    for (const argName of argNames) {
      if (err) break;
      if (Object.prototype.hasOwnProperty.call(argsObj, argName)) {
        if (argsObj[argName] !== undefined && argsObj[argName] !== null) {
          if (this.boolMap.has(argName)) {
            if (typeof argsObj[argName] === 'boolean') {
              argTokens.push(
                new ArgToken(this.boolMap.get(argName)!, ArgTokenType.Bool, argsObj[argName]),
              );
            } else {
              err = new FindError(`Invalid value for option: ${argName}`);
            }
          } else if (this.strMap.has(argName)) {
            if (typeof argsObj[argName] === 'string') {
              argTokens.push(
                new ArgToken(this.strMap.get(argName)!, ArgTokenType.Str, argsObj[argName]),
              );
            } else if (typeof argsObj[argName] === 'object' && Array.isArray(argsObj[argName])) {
              argsObj[argName].forEach((s) => {
                if (typeof s === 'string') {
                  argTokens.push(new ArgToken(this.strMap.get(argName)!, ArgTokenType.Str, s));
                } else {
                  err = new FindError(`Invalid value for option: ${argName}`);
                }
              });
            } else {
              err = new FindError(`Invalid value for option: ${argName}`);
            }
          } else if (this.intMap.has(argName)) {
            if (typeof argsObj[argName] === 'number') {
              argTokens.push(
                new ArgToken(this.intMap.get(argName)!, ArgTokenType.Int, argsObj[argName]),
              );
            } else {
              let { err, argToken } = this.tokenizeIntArg(argName, argsObj[argName]);
              if (err) break;
              argTokens.push(argToken!);
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

  public tokenizeJson(json: string): ArgTokenizerResult {
    let err: Error | undefined;
    const argTokens: ArgToken[] = [];
    try {
      const obj = JSON.parse(json);
      return this.tokenizeArgsObj(obj);
    } catch (e) {
      if (e instanceof SyntaxError) {
        err = new FindError(`Invalid JSON in settings: ${e.message}`);
      } else {
        err = new FindError(`Error parsing settings JSON: ${e}`);
      }
      return { err, argTokens };
    }
  }

  public tokenizeFile(filePath: string): ArgTokenizerResult {
    let err: Error | undefined;
    const argTokens: ArgToken[] = [];
    const expandedPath = FileUtil.expandPath(filePath);
    if (fs.existsSync(expandedPath)) {
      if (expandedPath.endsWith('.json')) {
        const json = FileUtil.getFileContentsSync(expandedPath, 'utf-8');
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
