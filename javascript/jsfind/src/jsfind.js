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

function handleError(err, findOptions) {
    const errMsg = 'ERROR: ' + err.message;
    common.logError('\n' + errMsg + '\n');
    findOptions.usageWithCode(1);
}

function getMatchingDirs(fileResults) {
    const dirs = fileResults.map(f => f.path);
    return common.setFromArray(dirs);
}

function printMatchingDirs(fileResults, formatter) {
    const dirs = getMatchingDirs(fileResults);
    if (dirs.length > 0) {
        common.log(`\nMatching directories (${dirs.length}):`);
        dirs.forEach(d => common.log(formatter.formatPath(d)));
    } else {
        common.log('\nMatching directories: 0');
    }
}

function printMatchingFiles(fileResults, formatter) {
    if (fileResults.length > 0) {
        common.log(`\nMatching files (${fileResults.length}):`);
        fileResults.forEach(fr => common.log(formatter.formatFileResult(fr)));
    } else {
        common.log('\nMatching files: 0');
    }
}

const findMain = async () => {
    const findOptions = new FindOptions();
    const args = process.argv.slice(2);

    findOptions.settingsFromArgs(args, async (err, settings) => {
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
            const finder = new Finder(settings);
            let fileResults = await finder.find();
            const formatter = new FileResultFormatter(settings);

            if (settings.printDirs) {
                printMatchingDirs(fileResults, formatter);
            }
            if (settings.printFiles) {
                printMatchingFiles(fileResults, formatter);
            }

        } catch (err2) {
            handleError(err2, findOptions);
        }
    });
};

// node.js equivalent of python's if __name__ == '__main__'
if (require.main === module) {
    findMain().catch((err) => common.log(err));
}
