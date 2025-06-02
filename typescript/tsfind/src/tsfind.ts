/*
 * tsfind.ts
 *
 * file find utility written in typescript
 */

'use strict';

import * as common from './common';
import {FindOptions} from './findoptions';
import {FindSettings} from './findsettings';
import {Finder} from './finder';
import {FileResult} from "./fileresult";
import {FileResultFormatter} from "./fileresultformatter";

function handleError(err: Error | any, findOptions: FindOptions) {
    const errMsg: string = 'ERROR: ' + err.message;
    common.logError('\n' + errMsg + '\n');
    findOptions.usageWithCode(1);
}

function getMatchingDirs(fileResults: FileResult[]): string[] {
    const dirs: string[] = fileResults.map(f => f.path);
    return common.setFromArray(dirs);
}

function printMatchingDirs(fileResults: FileResult[], formatter: FileResultFormatter): void {
    const dirs: string[] = getMatchingDirs(fileResults);
    if (dirs.length > 0) {
        common.log("\nMatching directories " + `(${dirs.length}):`);
        dirs.forEach(d => common.log(formatter.formatPath(d)));
    } else {
        common.log("\nMatching directories: 0");
    }
}

function printMatchingFiles(fileResults: FileResult[], formatter: FileResultFormatter): void {
    if (fileResults.length > 0) {
        common.log("\nMatching files " + `(${fileResults.length}):`);
        fileResults.forEach(f => common.log(formatter.formatFileResult(f)));
    } else {
        common.log("\nMatching files: 0");
    }
}

function findMain() {
    const findOptions = new FindOptions();
    const args = process.argv.slice(2);

    findOptions.settingsFromArgs(args, async (err: Error | void, settings: FindSettings) => {
        if (err) {
            handleError(err, findOptions);
        }

        if (settings.debug)
            common.log('settings: ' + settings.toString());

        if (settings.printUsage) {
            common.log('');
            findOptions.usage();
        }

        if (settings.printVersion) {
            common.log('Version: 0.1');
            process.exit(0);
        }

        try {
            const finder: Finder = new Finder(settings);
            const fileResults = await finder.find();
            const formatter = new FileResultFormatter(settings);

            if (settings.printDirs) {
                printMatchingDirs(fileResults, formatter);
            }
            if (settings.printFiles) {
                printMatchingFiles(fileResults, formatter);
            }

        } catch (err2) {
            handleError(err2 as Error, findOptions);
        }
    });
}

// node.js equivalent of python's if __name__ == '__main__'
if (require.main === module) {
    findMain();
}
