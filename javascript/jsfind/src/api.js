/**
 * @fileoverview Expose classes to require.
 * @author Cary Clark
 */

//-----------------------------------------------------------------------------
// Requirements
//-----------------------------------------------------------------------------

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
const {SortBy, nameToSortBy, sortByToName} = require('./sortby');

//-----------------------------------------------------------------------------
// Exports
//-----------------------------------------------------------------------------

module.exports = {
  COLORS: ConsoleColor,
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
  nameToSortBy,
  sortByToName
};
