/*
 * config.js
 *
 * Configuration values
 */

"use strict";

const config = require('../../../shared/config.json');

const isWin = /^win/.test(process.platform);

const HOME_NAME = isWin ? 'USERPROFILE' : 'HOME';
const HOME = process.env[HOME_NAME];

exports.XFINDPATH = config.xfindpath;
exports.SHAREDPATH = exports.XFINDPATH + '/shared';
exports.FILETYPESJSONPATH = __dirname + '/../data/filetypes.json';
exports.FINDOPTIONSJSONPATH = __dirname + '/../data/findoptions.json';
