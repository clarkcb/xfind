/**
 * @fileoverview Expose classes to require.
 * @author Cary Clark
 */

//-----------------------------------------------------------------------------
// Requirements
//-----------------------------------------------------------------------------

const {ArgToken} = require('./argtoken');
const {ArgTokenType} = require('./argtokentype');
const {ArgTokenizer} = require('./argtokenizer');
const {Color, colorToConsoleColor, nameToColor} = require('./color');
const {ConsoleColor} = require('./consolecolor');
const common = require('./common');
const {FileResult} = require('./fileresult');
const {FileResultFormatter} = require('./fileresultformatter');
const {FileResultSorter} = require('./fileresultsorter');
const {FileType} = require('./filetype');
const {FileTypes} = require('./filetypes');
const {FileUtil} = require('./fileutil');
const {FindOptions} = require('./findoptions');
const {FindSettings} = require('./findsettings');
const {Finder} = require('./finder');
const {FindError} = require('./finderror');
const StringUtil = require('./stringutil');
const {SortBy, nameToSortBy} = require('./sortby');

//-----------------------------------------------------------------------------
// Exports
//-----------------------------------------------------------------------------

module.exports = {
  ArgToken,
  ArgTokenType,
  ArgTokenizer,
  Color,
  ConsoleColor,
  common,
  FileResult,
  FileResultFormatter,
  FileResultSorter,
  FileType,
  FileTypes,
  FileUtil,
  Finder,
  FindError,
  FindOptions,
  FindSettings,
  StringUtil,
  SortBy,
  colorToConsoleColor,
  nameToColor,
  nameToSortBy
};
