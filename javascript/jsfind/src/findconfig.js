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
    const xfindPath = process.env.XFIND_PATH
      ? process.env.XFIND_PATH
      : path.join(HOME, 'src', 'xfind');
    // var sharedPath = path.join(xfind_path, 'shared');
    const jsfindPath = path.join(xfindPath, 'javascript', 'jsfind');
    const dataPath = path.join(jsfindPath, 'data');
    const fileTypesPath = path.join(dataPath, 'filetypes.json');
    const findOptionsPath = path.join(dataPath, 'findoptions.json');
    const defaultFindSettingsPath = path.join(HOME, '.config', 'xfind', 'settings.json');

    this.xfindPath = xfindPath;
    this.fileTypesPath = fileTypesPath;
    this.findOptionsPath = findOptionsPath;
    this.defaultFindSettingsPath = defaultFindSettingsPath;
  }
}

exports.FindConfig = FindConfig;
