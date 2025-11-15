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
import {FileResultFormatter} from "./fileresultformatter";

function handleError(err: Error | any, colorize: boolean, findOptions: FindOptions) {
    const errMsg: string = 'ERROR: ' + err.message;
    common.logError('\n' + errMsg + '\n', colorize);
    findOptions.usageWithCode(1);
}

function findMain() {
    const findOptions = new FindOptions();
    const args = process.argv.slice(2);

    findOptions.settingsFromArgs(args, async (err: Error | void, settings: FindSettings) => {
        if (err) {
            handleError(err, true, findOptions);
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
                finder.printMatchingDirs(fileResults, formatter);
            }
            if (settings.printFiles) {
                finder.printMatchingFiles(fileResults, formatter);
            }

        } catch (err2) {
            handleError(err2 as Error, settings.colorize, findOptions);
        }
    });
}

// node.js equivalent of python's if __name__ == '__main__'
if (require.main === module) {
    findMain();
}
