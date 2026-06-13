import 'dart:async';
import 'dart:io';

import 'package:dartfind/dartfind.dart';
import 'package:path/path.dart' as path;

const String startPathNotDefined = 'Startpath not defined';
const String startPathNotFound = 'Startpath not found';
const String startPathNotMatchFindSettings =
    'Startpath does not match find settings';
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
      if (path == null || path.trim().isEmpty) {
        throw FindException(startPathNotFound);
      }
      var p = path;
      var pathType =
          FileSystemEntity.typeSync(p, followLinks: settings.followSymlinks);
      if (pathType == FileSystemEntityType.notFound) {
        p = FileUtil.expandPath(p);
        pathType =
            FileSystemEntity.typeSync(p, followLinks: settings.followSymlinks);
        if (pathType == FileSystemEntityType.notFound) {
          throw FindException(startPathNotFound);
        }
      }
      if (pathType == FileSystemEntityType.link) {
        // this means followSymlinks is false,
        throw FindException(startPathNotMatchFindSettings);
      } else if (pathType == FileSystemEntityType.directory) {
        if (!isTraversableDir(Directory(p))) {
          throw FindException(startPathNotMatchFindSettings);
        }
      } else if (pathType == FileSystemEntityType.file) {
        if (!isMatchingFile(File(p))) {
          throw FindException(startPathNotMatchFindSettings);
        }
      } else {
        // TODO: start path is unknown/invalid type
        throw FindException(startPathNotMatchFindSettings);
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

  bool _matchesAnyPattern(String s, Set<Pattern> patterns) {
    return patterns.any((p) => p.allMatches(s).isNotEmpty);
  }

  bool _anyMatchesAnyPattern(Iterable<String> elems, Set<Pattern> patterns) {
    return elems.any((elem) => _matchesAnyPattern(elem, patterns));
  }

  bool _emptyOrMatchesAnyPattern(String s, Set<Pattern> patterns) {
    return patterns.isEmpty || _matchesAnyPattern(s, patterns);
  }

  bool _emptyOrNotMatchesAnyPattern(String s, Set<Pattern> patterns) {
    return patterns.isEmpty || !_matchesAnyPattern(s, patterns);
  }

  bool _emptyOrAnyMatchesAnyPattern(
      Iterable<String> elems, Set<Pattern> patterns) {
    return patterns.isEmpty || _anyMatchesAnyPattern(elems, patterns);
  }

  bool _emptyOrNotAnyMatchesAnyPattern(
      Iterable<String> elems, Set<Pattern> patterns) {
    return patterns.isEmpty || !_anyMatchesAnyPattern(elems, patterns);
  }

  bool _emptyOrMatchesAnyString(String s, Set<String> strings) {
    return strings.isEmpty || strings.contains(s);
  }

  bool _emptyOrNotMatchesAnyString(String s, Set<String> strings) {
    return strings.isEmpty || !strings.contains(s);
  }

  bool _emptyOrMatchesAnyFileType(FileType s, Set<FileType> fileTypes) {
    return fileTypes.isEmpty || fileTypes.contains(s);
  }

  bool _emptyOrNotMatchesAnyFileType(FileType s, Set<FileType> fileTypes) {
    return fileTypes.isEmpty || !fileTypes.contains(s);
  }

  bool isMatchingDirByHidden(Directory dir) {
    return settings.includeHidden || !FileUtil.isHiddenPath(dir.path);
  }

  bool isMatchingDirByInPatterns(Directory dir) {
    var elems = FileUtil.pathElems(dir.path);
    return _emptyOrAnyMatchesAnyPattern(elems, settings.inDirPatterns);
  }

  bool isMatchingDirByOutPatterns(Directory dir) {
    var elems = FileUtil.pathElems(dir.path);
    return _emptyOrNotAnyMatchesAnyPattern(elems, settings.outDirPatterns);
  }

  bool isTraversableDir(Directory dir) {
    return isMatchingDirByHidden(dir) && isMatchingDirByOutPatterns(dir);
  }

  bool isMatchingDir(Directory dir) {
    return isMatchingDirByHidden(dir) &&
        isMatchingDirByInPatterns(dir) &&
        isMatchingDirByOutPatterns(dir);
  }

  bool isNullOrMatchingDir(Directory? dir) {
    return dir == null || isMatchingDir(dir);
  }

  bool isMatchingFileNameByHidden(String fileName) {
    return settings.includeHidden || !FileUtil.isHiddenName(fileName);
  }

  bool isMatchingArchiveExtension(String ext) {
    return _emptyOrMatchesAnyString(ext, settings.inArchiveExtensions) &&
        _emptyOrNotMatchesAnyString(ext, settings.outArchiveExtensions);
  }

  bool isMatchingArchiveExtensionForFile(File file) {
    if (settings.inArchiveExtensions.isNotEmpty ||
        settings.outArchiveExtensions.isNotEmpty) {
      var fileName = path.basename(file.path);
      var ext = FileUtil.extension(fileName);
      return isMatchingArchiveExtension(ext);
    }
    return true;
  }

  bool isMatchingArchiveFileName(String fileName) {
    return _emptyOrMatchesAnyPattern(
            fileName, settings.inArchiveFilePatterns) &&
        _emptyOrNotMatchesAnyPattern(fileName, settings.outArchiveFilePatterns);
  }

  bool isMatchingArchiveFileNameForFile(File file) {
    if (settings.inArchiveFilePatterns.isNotEmpty ||
        settings.outArchiveFilePatterns.isNotEmpty) {
      var fileName = path.basename(file.path);
      return isMatchingArchiveFileName(fileName);
    }
    return true;
  }

  bool isMatchingArchiveFile(File file) {
    return isNullOrMatchingDir(file.parent) &&
        isMatchingArchiveExtensionForFile(file) &&
        isMatchingArchiveFileNameForFile(file);
  }

  bool isMatchingArchiveFileResult(FileResult fr) {
    return isMatchingArchiveFile(fr.file);
  }

  bool isMatchingExtension(String ext) {
    return _emptyOrMatchesAnyString(ext, settings.inExtensions) &&
        _emptyOrNotMatchesAnyString(ext, settings.outExtensions);
  }

  bool isMatchingExtensionForFile(File file) {
    if (settings.inExtensions.isNotEmpty || settings.outExtensions.isNotEmpty) {
      var fileName = path.basename(file.path);
      var ext = FileUtil.extension(fileName);
      return isMatchingExtension(ext);
    }
    return true;
  }

  bool isMatchingFileName(String fileName) {
    return _emptyOrMatchesAnyPattern(fileName, settings.inFilePatterns) &&
        _emptyOrNotMatchesAnyPattern(fileName, settings.outFilePatterns);
  }

  bool isMatchingFileNameForFile(File file) {
    if (settings.inFilePatterns.isNotEmpty ||
        settings.outFilePatterns.isNotEmpty) {
      var fileName = path.basename(file.path);
      return isMatchingFileName(fileName);
    }
    return true;
  }

  bool isMatchingFile(File file) {
    return isNullOrMatchingDir(file.parent) &&
        isMatchingExtensionForFile(file) &&
        isMatchingFileNameForFile(file);
  }

  bool isMatchingFileType(FileType fileType) {
    return _emptyOrMatchesAnyFileType(fileType, settings.inFileTypes) &&
        _emptyOrNotMatchesAnyFileType(fileType, settings.outFileTypes);
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

  bool isMatchingFileResult(FileResult fr) {
    return isMatchingFile(fr.file) &&
        isMatchingFileType(fr.fileType) &&
        isMatchingFileSize(fr.fileSize) &&
        isMatchingLastMod(fr.lastMod);
  }

  (int, DateTime?) getFileSizeAndLastMod(File f) {
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
    return (fileSize, lastMod);
  }

  Future<FileResult?> filterArchiveFileToFileResult(File f) async {
    if (!settings.includeArchives && !settings.archivesOnly) {
      return Future.value(null);
    }

    if (!isMatchingArchiveFile(f)) {
      return Future.value(null);
    }

    int fileSize = 0;
    DateTime? lastMod;
    var fileResult = FileResult(f, FileType.archive, fileSize, lastMod);
    return Future.value(fileResult);
  }

  Future<FileResult?> filterRegularFileToFileResult(
      File f, FileType fileType) async {
    if (settings.archivesOnly) {
      return Future.value(null);
    }

    if (!isMatchingFile(f) || !isMatchingFileType(fileType)) {
      return Future.value(null);
    }

    var (fileSize, lastMod) = getFileSizeAndLastMod(f);

    if (!isMatchingFileSize(fileSize) || !isMatchingLastMod(lastMod)) {
      return Future.value(null);
    }

    var fileResult = FileResult(f, fileType, fileSize, lastMod);
    return Future.value(fileResult);
  }

  Future<FileResult?> filterToFileResult(File f) async {
    if (!isNullOrMatchingDir(f.parent)) {
      // return Future.value(null);
      return null;
    }

    var fileName = path.basename(f.path);
    if (!isMatchingFileNameByHidden(fileName)) {
      // return Future.value(null);
      return null;
    }

    var fileType = await _fileTypes.getFileType(fileName);
    if (fileType == FileType.archive) {
      return filterArchiveFileToFileResult(f);
    }
    return filterRegularFileToFileResult(f, fileType);
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
      // TODO: I think this is supposed to mean followSymlinks == false?
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
          isMatchingDirByHidden(entry as Directory) &&
          isMatchingDirByOutPatterns(entry)) {
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

  Future<List<FileResult>> _getFileResultsForPath(String aPath) async {
    var fileSysType = await FileSystemEntity.type(aPath,
        followLinks: settings.followSymlinks);
    if (fileSysType == FileSystemEntityType.notFound) {
      aPath = FileUtil.expandPath(aPath);
      fileSysType = await FileSystemEntity.type(aPath,
          followLinks: settings.followSymlinks);
      if (fileSysType == FileSystemEntityType.notFound) {
        throw FindException(startPathNotFound);
      }
    }

    if (fileSysType == FileSystemEntityType.link) {
      // this means followSymlinks is false,
      throw FindException(startPathNotMatchFindSettings);
    }

    if (fileSysType == FileSystemEntityType.directory) {
      // if max_depth is zero, we can skip since a directory cannot be a result
      if (settings.maxDepth == 0) {
        return [];
      }

      var dir = Directory(aPath);
      if (isTraversableDir(dir)) {
        var maxDepth = settings.maxDepth;
        if (!settings.recursive) {
          maxDepth = 1;
        }
        return _recGetFileResultsForDir(dir, settings.minDepth, maxDepth, 1);
      } else {
        throw FindException(startPathNotMatchFindSettings);
      }
    } else if (fileSysType == FileSystemEntityType.file) {
      // if min_depth > zero, we can skip since the file is at depth zero
      if (settings.minDepth > 0) {
        return [];
      }

      var startFile = File(aPath);
      var fileResult = await filterToFileResult(startFile);

      if (fileResult != null) {
        return [fileResult];
      } else {
        throw FindException(startPathNotMatchFindSettings);
      }
    } else {
      throw FindException(startPathNotMatchFindSettings);
    }
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
