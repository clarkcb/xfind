/*
 * config.js
 *
 * Configuration values
 */

"use strict";

const path = require('path');

const isWin = /^win/.test(process.platform);

const HOME_NAME = isWin ? 'USERPROFILE' : 'HOME';
const HOME = process.env[HOME_NAME];

exports.XFIND_PATH = process.env.XFIND_PATH ? process.env.XFIND_PATH : path.join(HOME, 'src', 'xfind');
exports.SHARED_PATH = path.join(exports.XFIND_PATH, 'shared');
const JSFIND_PATH = path.join(exports.XFIND_PATH, 'javascript', 'jsfind');
const DATA_PATH = path.join(JSFIND_PATH, 'data');
exports.FILE_TYPES_JSON_PATH = path.join(DATA_PATH, 'filetypes.json');
exports.FIND_OPTIONS_JSON_PATH = path.join(DATA_PATH, 'findoptions.json');
