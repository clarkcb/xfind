/*
 * config.ts
 *
 * Configuration values
 */

'use strict';

const config = require('../data/config.json');

const isWin: boolean = /^win/.test(process.platform);

const HOME_NAME: string = isWin ? 'USERPROFILE' : 'HOME';
export const HOME: string = process.env[HOME_NAME] || '';

// use XFIND_PATH env var if defined
if (process.env.XFIND_PATH) {
    config.xfindpath = process.env.XFIND_PATH;
}

export const XFINDPATH: string = config.xfindpath;
export const SHAREDPATH: string = XFINDPATH + '/shared';
export const FILETYPESJSONPATH: string = __dirname + '/../data/filetypes.json';
export const FINDOPTIONSJSONPATH: string = __dirname + '/../data/findoptions.json';
