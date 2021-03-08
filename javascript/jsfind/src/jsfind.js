/*
 * jsfind.js
 *
 * file find utility written in JavaScript + Node.js
 */

'use strict';

const common = require('./common');
const {Finder} = require('./finder');
const {FindOptions} = require('./findoptions');

function handleError(err, findOptions) {
    const errMsg = 'ERROR: ' + err.message;
    common.log('\n' + errMsg + '\n');
    findOptions.usageWithCode(1);
}

function getMatchingDirs(findfiles) {
    const dirs = findfiles.map(f => f.pathname);
    return common.setFromArray(dirs);
}

function printMatchingDirs(findfiles) {
    const dirs = getMatchingDirs(findfiles);
    if (dirs.length > 0) {
        common.log(`\nMatching directories (${dirs.length}):`);
        dirs.forEach(d => common.log(d));
    } else {
        common.log('\nMatching directories: 0');
    }
}

function getMatchingFiles(findfiles) {
    const files = findfiles.map(f => f.relativePath());
    return common.setFromArray(files);
}

function printMatchingFiles(findfiles) {
    const files = getMatchingFiles(findfiles);
    if (files.length > 0) {
        common.log(`\nMatching files (${files.length}):`);
        files.forEach(f => common.log(f));
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
            let findfiles = await finder.find();

            if (settings.listDirs) {
                printMatchingDirs(findfiles);
            }
            if (settings.listFiles) {
                printMatchingFiles(findfiles);
            }

        } catch (err2) {
            handleError(err2, findOptions);
        }
    });
};

// node.js equivalent of python's if __name__ == '__main__'
if (!module.parent) {
    findMain();
}
