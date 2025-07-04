/*
 * fileresultsorter.js
 *
 * FileResultSorter class provides sorting of search results
 */
const {SortBy} = require('./sortby');


class FileResultSorter {
  'use strict'

  constructor (settings) {
    this.settings = settings;
  }

  cmpFileResultsByPath(fr1, fr2) {
    const [path1, path2] = this.settings.sortCaseInsensitive ?
      [fr1.path.toLowerCase(), fr2.path.toLowerCase()] :
      [fr1.path, fr2.path];
    if (path1 === path2) {
      const [fileName1, fileName2] = this.settings.sortCaseInsensitive ?
        [fr1.fileName.toLowerCase(), fr2.fileName.toLowerCase()] :
        [fr1.fileName, fr2.fileName];
      if (fileName1 === fileName2) return 0;
      return fileName1 < fileName2 ? -1 : 1;
    }
    return path1 < path2 ? -1 : 1;
  }

  cmpFileResultsByName(fr1, fr2) {
    const [fileName1, fileName2] = this.settings.sortCaseInsensitive ?
      [fr1.fileName.toLowerCase(), fr2.fileName.toLowerCase()] :
      [fr1.fileName, fr2.fileName];
    if (fileName1 === fileName2) {
      const [path1, path2] = this.settings.sortCaseInsensitive ?
        [fr1.path.toLowerCase(), fr2.path.toLowerCase()] :
        [fr1.path, fr2.path];
      if (path1 === path2) return 0;
      return path1 < path2 ? -1 : 1;
    }
    return fileName1 < fileName2 ? -1 : 1;
  }

  cmpFileResultsBySize(fr1, fr2) {
    if (fr1.fileSize === fr2.fileSize) {
      return this.cmpFileResultsByPath(fr1, fr2);
    }
    return fr1.fileSize < fr2.fileSize ? -1 : 1;
  }

  cmpFileResultsByType(fr1, fr2) {
    if (fr1.fileType === fr2.fileType) {
      return this.cmpFileResultsByPath(fr1, fr2);
    }
    return fr1.fileType < fr2.fileType ? -1 : 1;
  }

  cmpFileResultsByLastMod(fr1, fr2) {
    if (fr1.lastMod === fr2.lastMod) {
      return this.cmpFileResultsByPath(fr1, fr2);
    }
    return fr1.lastMod < fr2.lastMod ? -1 : 1;
  }

  getFileResultComparator() {
    if (this.settings.sortDescending) {
      switch (this.settings.sortBy) {
        case SortBy.FILENAME:
          return (a, b) => this.cmpFileResultsByName(b, a);
        case SortBy.FILESIZE:
          return (a, b) => this.cmpFileResultsBySize(b, a);
        case SortBy.FILETYPE:
          return (a, b) => this.cmpFileResultsByType(b, a);
        case SortBy.LASTMOD:
          return (a, b) => this.cmpFileResultsByLastMod(b, a);
        default:
          return (a, b) => this.cmpFileResultsByPath(b, a);
      }
    }
    switch (this.settings.sortBy) {
      case SortBy.FILENAME:
        return (a, b) => this.cmpFileResultsByName(a, b);
      case SortBy.FILESIZE:
        return (a, b) => this.cmpFileResultsBySize(a, b);
      case SortBy.FILETYPE:
        return (a, b) => this.cmpFileResultsByType(a, b);
      case SortBy.LASTMOD:
        return (a, b) => this.cmpFileResultsByLastMod(a, b);
      default:
        return (a, b) => this.cmpFileResultsByPath(a, b);
    }
  }

  sort(fileResults) {
    let sortComparator = this.getFileResultComparator();
    fileResults.sort(sortComparator);
  }
}

exports.FileResultSorter = FileResultSorter;
