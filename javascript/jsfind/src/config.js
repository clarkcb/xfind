/*
 * config.js
 *
 * Configuration values
 */

"use strict";

const isWin = /^win/.test(process.platform);

const HOME_NAME = isWin ? 'USERPROFILE' : 'HOME';
const HOME = process.env[HOME_NAME];

exports.XFIND_PATH = process.env.XFIND_PATH ? process.env.XFIND_PATH : `${HOME}/src/xfind`;
exports.SHARED_PATH = `${exports.XFIND_PATH}/shared`;
const JSFIND_PATH = `${exports.XFIND_PATH}/javascript/jsfind`;
const DATA_PATH = `${JSFIND_PATH}/data`;
exports.FILE_TYPES_JSON_PATH = `${DATA_PATH}/filetypes.json`;
exports.FIND_OPTIONS_JSON_PATH = `${DATA_PATH}/findoptions.json`;
