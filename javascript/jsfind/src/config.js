/*
 * config.js
 *
 * Configuration values
 */

'use strict';

const config = require('../data/config.json');

// const isWin = /^win/.test(process.platform);

// const HOME_NAME = isWin ? 'USERPROFILE' : 'HOME';
// const HOME = process.env[HOME_NAME];

// use XFIND_PATH env var if defined
if (process.env.XFIND_PATH) {
    config.xfindpath = process.env.XFIND_PATH;
}

exports.XFINDPATH = config.xfindpath;
exports.SHAREDPATH = exports.XFINDPATH + '/shared';
exports.FILETYPESJSONPATH = __dirname + '/../data/filetypes.json';
exports.FINDOPTIONSJSONPATH = __dirname + '/../data/findoptions.json';
