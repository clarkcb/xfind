/*
 * config.ts
 *
 * Configuration values
 */

'use strict';

import * as path from 'path';

const isWin: boolean = /^win/.test(process.platform);

const HOME_NAME: string = isWin ? 'USERPROFILE' : 'HOME';
export const HOME: string = process.env[HOME_NAME] || '';

export const XFIND_PATH: string = process.env.XFIND_PATH ? process.env.XFIND_PATH : path.join(HOME, 'src', 'xfind');
export const SHARED_PATH = path.join(XFIND_PATH, 'shared');
const TSFIND_PATH = path.join(XFIND_PATH, 'typescript', 'tsfind');
const DATA_PATH = path.join(TSFIND_PATH, 'data');
export const FILE_TYPES_JSON_PATH: string = path.join(DATA_PATH, 'filetypes.json');
export const FIND_OPTIONS_JSON_PATH: string = path.join(DATA_PATH, 'findoptions.json');
