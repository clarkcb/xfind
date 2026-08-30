/*
 * findconfig.js
 *
 * Configuration values
 */

'use strict';

const path = require('path');

const isWin = /^win/.test(process.platform);

const HOME_NAME = isWin ? 'USERPROFILE' : 'HOME';
const HOME = process.env[HOME_NAME];

class FindConfig {
  constructor() {
    const defaultXFindConfigDir = path.join(HOME, '.config', 'xfind');
    const xfindConfigDir = process.env.XFIND_CONFIG_DIR
      ? process.env.XFIND_CONFIG_DIR
      : defaultXFindConfigDir;
    const defaultXFindPath = path.join(HOME, 'src', 'xfind');
    const xfindPath = process.env.XFIND_PATH ? process.env.XFIND_PATH : defaultXFindPath;
    const jsfindPath = path.join(xfindPath, 'javascript', 'jsfind');
    const dataPath = path.join(jsfindPath, 'data');
    const fileTypesPath = path.join(dataPath, 'filetypes.json');
    const findOptionsPath = path.join(dataPath, 'findoptions.json');
    const defaultFindSettingsPath = path.join(xfindConfigDir, 'settings.json');

    this.xfindPath = xfindPath;
    this.fileTypesPath = fileTypesPath;
    this.findOptionsPath = findOptionsPath;
    this.defaultFindSettingsPath = defaultFindSettingsPath;
  }
}

exports.FindConfig = FindConfig;
