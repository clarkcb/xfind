/*
 * config.js
 *
 * Configuration values
 */

const isWin = /^win/.test(process.platform);

const HOME_NAME = isWin ? 'USERPROFILE' : 'HOME';
const HOME = process.env[HOME_NAME];

exports.XFINDPATH = process.env.XFIND_PATH ? process.env.XFIND_PATH : `${HOME}/src/xfind`;
exports.SHAREDPATH = `${exports.XFINDPATH}/shared`;
const JSFINDPATH = `${exports.XFINDPATH}/javascript/jsfind`;
const DATAPATH = `${JSFINDPATH}/data`;
exports.FILETYPESJSONPATH = `${DATAPATH}/filetypes.json`;
exports.FINDOPTIONSJSONPATH = `${DATAPATH}/findoptions.json`;
