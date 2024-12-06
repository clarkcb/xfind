/*
 * config.ts
 *
 * Configuration values
 */

'use strict';

const isWin: boolean = /^win/.test(process.platform);

const HOME_NAME: string = isWin ? 'USERPROFILE' : 'HOME';
export const HOME: string = process.env[HOME_NAME] || '';

export const XFIND_PATH: string = process.env.XFIND_PATH ? process.env.XFIND_PATH : `${HOME}/src/xfind`;
export const SHARED_PATH = `${XFIND_PATH}/shared`;
const TSFIND_PATH = `${XFIND_PATH}/typescript/tsfind`;
const DATA_PATH = `${TSFIND_PATH}/data`;
export const FILE_TYPES_JSON_PATH: string = `${DATA_PATH}/filetypes.json`;
export const FIND_OPTIONS_JSON_PATH: string = `${DATA_PATH}/findoptions.json`;
