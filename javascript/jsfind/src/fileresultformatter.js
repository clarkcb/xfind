/*
 * fileresultformatter.js
 *
 * FileResultFormatter class provides formatting of search result instances
 */
const {ConsoleColor} = require('./consolecolor');
const path = require('path');

class FileResultFormatter {
  'use strict'

  constructor(settings) {
    this.settings = settings;
    if (settings.colorize) {
      if (settings.inDirPatterns.length > 0) {
        this.formatPath = function(path) {
          return this.formatPathWithColor(path);
        };
      }
      if (settings.inExtensions.length > 0 || settings.inFilePatterns.length > 0) {
        this.formatFileName = function(fileName) {
          return this.formatFileNameWithColor(fileName);
        };
      }
    }
  }

  colorize(s, matchStartIndex, matchEndIndex) {
    let prefix = ''
    if (matchStartIndex > 0) {
      prefix = s.slice(0, matchStartIndex);
    }
    let suffix = ''
    if (matchEndIndex < s.length) {
      suffix = s.slice(matchEndIndex);
    }
    return prefix +
      ConsoleColor.GREEN +
      s.slice(matchStartIndex, matchEndIndex) +
      ConsoleColor.RESET +
      suffix;
  }

  formatPathWithColor(path) {
    let formattedPath = '.'
    if (path) {
      formattedPath = path;
      for (let p of this.settings.inDirPatterns) {
        let m = p.exec(formattedPath);
        if (m) {
          formattedPath = this.colorize(formattedPath, m.index, m.index + m[0].length);
          break;
        }
      }
    }
    return formattedPath;
  }

  formatPath(path) {
    return path;
  }

  formatFileNameWithColor(fileName) {
    let formattedFileName = fileName;
    for (let p of this.settings.inFilePatterns) {
      let m = p.exec(formattedFileName);
      if (m) {
        formattedFileName = this.colorize(formattedFileName, m.index, m.index + m[0].length);
        break;
      }
    }
    if (this.settings.inExtensions.length > 0) {
      let idx = formattedFileName.lastIndexOf('.');
      if (idx > 0 && idx < formattedFileName.length - 1) {
        formattedFileName = this.colorize(formattedFileName, idx + 1, formattedFileName.length);
      }
    }
    return formattedFileName;
  }

  formatFileName(fileName) {
    return fileName;
  }

  formatFileResult(result) {
    let parent = this.formatPath(result.path);
    let fileName = this.formatFileName(result.fileName);
    return path.join(parent, fileName);
  }
}

exports.FileResultFormatter = FileResultFormatter;
