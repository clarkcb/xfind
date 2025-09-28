import 'dart:async';
import 'dart:io';

import 'package:dartfind/dartfind.dart';
import 'package:path/path.dart' as path;

const String startPathNotDefined = 'Startpath not defined';
const String startPathNotFound = 'Startpath not found';
const String invalidRangeDepth = 'Invalid range for mindepth and maxdepth';
const String invalidRangeLastMod =
    'Invalid range for minlastmod and maxlastmod';
const String invalidRangeSize = 'Invalid range for minsize and maxsize';

class Finder {
  final FindSettings settings;
  final FileTypes _fileTypes = FileTypes();
  // whether to allow invalid/malformed characters in encoding/decoding
  // setting to false means find will end once an attempt is made to read
  // a file in an incompatible encoding
  final bool allowInvalid = true;
  Finder(this.settings) {
    _validateSettings();
  }

  void _validateSettings() {
    if (settings.paths.isEmpty) {
      throw FindException(startPathNotDefined);
    }
    for (var path in settings.paths) {
      var p = path;
      if (FileSystemEntity.typeSync(path!) == FileSystemEntityType.notFound) {
        p = FileUtil.expandPath(path);
      }
      var pathType = FileSystemEntity.typeSync(p!);
      if (pathType == FileSystemEntityType.notFound) {
        throw FindException(startPathNotFound);
      }
    }
    if (settings.maxDepth > -1 &&
        settings.minDepth > -1 &&
        (settings.maxDepth < settings.minDepth)) {
      throw FindException(invalidRangeDepth);
    }
    if (settings.maxLastMod != null &&
        settings.minLastMod != null &&
        (settings.maxLastMod!.millisecondsSinceEpoch <
            settings.minLastMod!.millisecondsSinceEpoch)) {
      throw FindException(invalidRangeLastMod);
    }
    if (settings.maxSize > 0 &&
        settings.minSize > 0 &&
        (settings.maxSize < settings.minSize)) {
      throw FindException(invalidRangeSize);
    }
  }

  bool _anyMatchesAnyPattern(Iterable<String> elems, Set<Pattern> patterns) {
    return elems.any((elem) => _matchesAnyPattern(elem, patterns));
  }

  bool _matchesAnyPattern(String s, Set<Pattern> patterns) {
    return patterns.any((p) => p.allMatches(s).isNotEmpty);
  }

  bool filterDirByHidden(Directory dir) {
    return settings.includeHidden || !FileUtil.isHiddenPath(dir.path);
  }

  bool filterDirByInPatterns(Directory dir) {
    var elems = FileUtil.pathElems(dir.path);
    return (settings.inDirPatterns.isEmpty ||
        _anyMatchesAnyPattern(elems, settings.inDirPatterns));
  }

  bool filterDirByOutPatterns(Directory dir) {
    var elems = FileUtil.pathElems(dir.path);
    return (settings.outDirPatterns.isEmpty ||
        !_anyMatchesAnyPattern(elems, settings.outDirPatterns));
  }

  bool isMatchingDir(Directory dir) {
    return filterDirByHidden(dir) &&
        filterDirByInPatterns(dir) &&
        filterDirByOutPatterns(dir);
  }

  bool isMatchingExtension(
      String ext, Set<String> inExtensions, Set<String> outExtensions) {
    return (inExtensions.isEmpty || inExtensions.contains(ext)) &&
        (outExtensions.isEmpty || !outExtensions.contains(ext));
  }

  bool hasMatchingArchiveExtension(FileResult fr) {
    var fileName = path.basename(fr.file.path);
    if (settings.inArchiveExtensions.isNotEmpty ||
        settings.outArchiveExtensions.isNotEmpty) {
      var ext = FileUtil.extension(fileName);
      return isMatchingExtension(
          ext, settings.inArchiveExtensions, settings.outArchiveExtensions);
    }
    return true;
  }

  bool hasMatchingExtension(FileResult fr) {
    if (settings.inExtensions.isNotEmpty || settings.outExtensions.isNotEmpty) {
      var fileName = path.basename(fr.file.path);
      var ext = FileUtil.extension(fileName);
      return isMatchingExtension(
          ext, settings.inExtensions, settings.outExtensions);
    }
    return true;
  }

  bool isMatchingFileName(
      String fileName, Set<Pattern> inPatterns, Set<Pattern> outPatterns) {
    return (inPatterns.isEmpty || _matchesAnyPattern(fileName, inPatterns)) &&
        (outPatterns.isEmpty || !_matchesAnyPattern(fileName, outPatterns));
  }

  bool hasMatchingArchiveFileName(FileResult fr) {
    var fileName = path.basename(fr.file.path);
    return isMatchingFileName(fileName, settings.inArchiveFilePatterns,
        settings.outArchiveFilePatterns);
  }

  bool hasMatchingFileName(FileResult fr) {
    var fileName = path.basename(fr.file.path);
    return isMatchingFileName(
        fileName, settings.inFilePatterns, settings.outFilePatterns);
  }

  bool isMatchingFileType(FileType fileType) {
    return (settings.inFileTypes.isEmpty ||
            settings.inFileTypes.contains(fileType)) &&
        (settings.outFileTypes.isEmpty ||
            !settings.outFileTypes.contains(fileType));
  }

  bool isMatchingFileSize(int fileSize) {
    return ((settings.maxSize == 0 || fileSize <= settings.maxSize) &&
        (settings.minSize == 0 || fileSize >= settings.minSize));
  }

  bool isMatchingLastMod(DateTime? lastMod) {
    return ((settings.maxLastMod == null ||
            lastMod!.compareTo(settings.maxLastMod!) <= 0) &&
        (settings.minLastMod == null ||
            lastMod!.compareTo(settings.minLastMod!) >= 0));
  }

  bool isMatchingArchiveFileResult(FileResult fr) {
    return hasMatchingArchiveExtension(fr) && hasMatchingArchiveFileName(fr);
  }

  bool isMatchingFileResult(FileResult fr) {
    return hasMatchingExtension(fr) &&
        hasMatchingFileName(fr) &&
        isMatchingFileType(fr.fileType) &&
        isMatchingFileSize(fr.fileSize) &&
        isMatchingLastMod(fr.lastMod);
  }

  Future<FileResult?> filterToFileResult(File f) async {
    if (!isMatchingDir(f.parent)) {
      return Future.value(null);
    }
    var fileName = path.basename(f.path);
    if (!settings.includeHidden && FileUtil.isHiddenName(fileName)) {
      return Future.value(null);
    }
    int fileSize = 0;
    DateTime? lastMod;
    if (settings.needSize() || settings.needLastMod()) {
      FileStat stat = FileStat.statSync(f.path);
      if (settings.needSize()) {
        fileSize = stat.size;
      }
      if (settings.needLastMod()) {
        lastMod = stat.modified;
      }
    }
    var fileType = await _fileTypes.getFileType(fileName);
    var fileResult = FileResult(f, fileType, fileSize, lastMod);
    if (fileResult.fileType == FileType.archive) {
      if (settings.includeArchives) {
        return fileResult;
      }
      return null;
    }
    if (!settings.archivesOnly && isMatchingFileResult(fileResult)) {
      return fileResult;
    }
    return null;
  }

  Future<List<FileResult>> filterToFileResults(List<File> filePaths) {
    var fileResultsFutures = filePaths.map((f) => filterToFileResult(f));
    return Future.wait(fileResultsFutures).then((fileResults) {
      return fileResults.where((fr) => fr != null).toList().cast<FileResult>();
    });
  }

  Future<List<FileResult>> _recGetFileResultsForDir(
      Directory dir, int minDepth, int maxDepth, int currentDepth) async {
    var fileResults = <FileResult>[];
    var recurse = true;
    if (currentDepth == maxDepth) {
      recurse = false;
    } else if (maxDepth > -1 && currentDepth > maxDepth) {
      return [];
    }

    // Get dirs and files under filePath
    var pathDirs = <Directory>[];
    var pathFiles = <File>[];
    List<FileSystemEntity> entries = dir
        .listSync(recursive: false, followLinks: settings.followSymlinks)
        .toList();
    for (var entry in entries) {
      var linkIsDir = false;
      var linkIsFile = false;
      if (entry is Link) {
        var resolvedPath = entry.resolveSymbolicLinksSync();
        if (resolvedPath is Directory) {
          linkIsDir = true;
        } else if (resolvedPath is File) {
          linkIsFile = true;
        }
      }
      if ((entry is Directory || linkIsDir) &&
          recurse &&
          filterDirByHidden(entry as Directory) &&
          filterDirByOutPatterns(entry as Directory)) {
        pathDirs.add(entry);
      } else if ((entry is File || linkIsFile) &&
          (minDepth < 0 || currentDepth >= minDepth)) {
        pathFiles.add(entry as File);
      }
    }

    // Filter files
    var pathResults = await filterToFileResults(pathFiles);
    fileResults.addAll(pathResults);

    // Recurse into dirs
    for (var pathDir in pathDirs) {
      var subFileResults = await _recGetFileResultsForDir(
          pathDir, minDepth, maxDepth, currentDepth + 1);
      fileResults.addAll(subFileResults);
    }
    return fileResults;
  }

  Future<List<FileResult>> _getFileResultsForPath(String filePath) async {
    if (FileSystemEntity.typeSync(filePath) == FileSystemEntityType.notFound) {
      filePath = FileUtil.expandPath(filePath);
    }

    return FileSystemEntity.type(filePath).then((fsType) {
      if (fsType == FileSystemEntityType.directory) {
        // if max_depth is zero, we can skip since a directory cannot be a result
        if (settings.maxDepth == 0) {
          return [];
        }
        var dir = Directory(filePath);
        var maxDepth = settings.maxDepth;
        if (!settings.recursive) {
          maxDepth = 1;
        }
        return _recGetFileResultsForDir(dir, settings.minDepth, maxDepth, 1);
      } else {
        // if min_depth > zero, we can skip since the file is at depth zero
        if (settings.minDepth > 0) {
          return [];
        }
        var startFile = File(filePath);
        if (filterDirByHidden(startFile.parent) &&
            filterDirByOutPatterns(startFile.parent)) {
          int fileSize = 0;
          DateTime? lastMod;
          if (settings.needSize() || settings.needLastMod()) {
            FileStat stat = FileStat.statSync(filePath);
            if (settings.needSize()) {
              fileSize = stat.size;
            }
            if (settings.needLastMod()) {
              lastMod = stat.modified;
            }
          }
          return _fileTypes.getFileType(filePath).then((fileType) {
            var fileResult = FileResult(startFile, fileType, fileSize, lastMod);
            if (isMatchingFileResult(fileResult)) {
              return [fileResult];
            }
            return [];
          });
        }
        return [];
      }
    });
  }

  Future<List<FileResult>> _findFiles() async {
    var fileResults = <FileResult>[];
    var pathsFileResultsFutures =
        settings.paths.map((p) => _getFileResultsForPath(p!));
    await Future.wait(pathsFileResultsFutures).then((pathsFileResults) {
      for (var pathFileResults in pathsFileResults) {
        fileResults.addAll(pathFileResults);
      }
    });
    if (fileResults.length > 1) {
      var fileResultSorter = FileResultSorter(settings);
      fileResultSorter.sort(fileResults);
    }
    return fileResults;
  }

  Future<List<FileResult>> find() async {
    return Future.wait([_fileTypes.ready]).then((res) {
      return _findFiles();
    });
  }

  List<Directory> getMatchingDirs(List<FileResult> fileResults) {
    Map<String, Directory> pathDirMap = {};
    for (var fr in fileResults) {
      var dir = fr.file.parent;
      pathDirMap[dir.path] = dir;
    }
    var paths = pathDirMap.keys.toList();
    paths.sort();
    List<Directory> dirs = [];
    for (var p in paths) {
      var dir = pathDirMap[p];
      dirs.add(dir!);
    }
    return dirs;
  }

  void printMatchingDirs(
      List<FileResult> fileResults, FileResultFormatter formatter) {
    var dirs = getMatchingDirs(fileResults);
    if (dirs.isNotEmpty) {
      logMsg('\nMatching directories (${dirs.length}):');
      for (var d in dirs) {
        logMsg(formatter.formatDirectory(d));
      }
    } else {
      logMsg('\nMatching directories: 0');
    }
  }

  void printMatchingFiles(
      List<FileResult> fileResults, FileResultFormatter formatter) {
    if (fileResults.isNotEmpty) {
      logMsg('\nMatching files (${fileResults.length}):');
      for (var fr in fileResults) {
        logMsg(formatter.formatFileResult(fr));
      }
    } else {
      logMsg('\nMatching files: 0');
    }
  }
}
