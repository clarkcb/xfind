/*jslint node: true */
/*
 * jsfind.js
 *
 * file find utility written in JavaScript + Node.js
 */

'use strict';

const common = require('./common');
const {FileResultFormatter} = require('./fileresultformatter');
const {Finder} = require('./finder');
const {FindOptions} = require('./findoptions');

function handleError(err, colorize, findOptions) {
    const errMsg = 'ERROR: ' + err.message;
    common.logError('\n' + errMsg + '\n', colorize);
    findOptions.usageWithCode(1);
}

const findMain = async () => {
    const findOptions = new FindOptions();
    const args = process.argv.slice(2);

    findOptions.settingsFromArgs(args, async (err, settings) => {
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
            const finder = new Finder(settings);
            let fileResults = await finder.find();
            const formatter = new FileResultFormatter(settings);

            if (settings.printDirs) {
                finder.printMatchingDirs(fileResults, formatter);
            }
            if (settings.printFiles) {
                finder.printMatchingFiles(fileResults, formatter);
            }

        } catch (err2) {
            handleError(err2, settings.colorize, findOptions);
        }
    });
};

// node.js equivalent of python's if __name__ == '__main__'
if (require.main === module) {
    findMain().catch((err) => common.logError(err));
}
