/*
 * config.ts
 *
 * Configuration values
 */

'use strict';


const isWin: boolean = /^win/.test(process.platform);

const HOME_NAME: string = isWin ? 'USERPROFILE' : 'HOME';
export const HOME: string = process.env[HOME_NAME] || '';

export const XFINDPATH: string = process.env.XFIND_PATH ? process.env.XFIND_PATH : `${HOME}/src/xfind`;
export const SHAREDPATH = `${XFINDPATH}/shared`;
const TSFINDPATH = `${XFINDPATH}/typescript/tsfind`;
const DATAPATH = `${TSFINDPATH}/data`;
export const FILETYPESJSONPATH: string = `${DATAPATH}/filetypes.json`;
export const FINDOPTIONSJSONPATH: string = `${DATAPATH}/findoptions.json`;
