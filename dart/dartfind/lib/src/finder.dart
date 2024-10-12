import 'dart:async';
import 'dart:io';

import 'package:dartfind/src/file_result.dart';
import 'package:dartfind/src/file_types.dart';
import 'package:dartfind/src/file_util.dart';
import 'package:dartfind/src/find_exception.dart';
import 'package:dartfind/src/find_settings.dart';
import 'package:path/path.dart' as path;

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
      throw FindException('Startpath not defined');
    }
    for (var p in settings.paths) {
      var startPath = FileUtil.expandPath(p!);
      if (FileSystemEntity.typeSync(startPath) ==
          FileSystemEntityType.notFound) {
        throw FindException('Startpath not found');
      }
    }
    if (settings.maxDepth > -1 &&
        settings.minDepth > -1 &&
        (settings.maxDepth < settings.minDepth)) {
      throw FindException('Invalid range for mindepth and maxdepth');
    }
    if (settings.maxLastMod != null &&
        settings.minLastMod != null &&
        (settings.maxLastMod!.millisecondsSinceEpoch <
            settings.minLastMod!.millisecondsSinceEpoch)) {
      throw FindException('Invalid range for minlastmod and maxlastmod');
    }
    if (settings.maxSize > 0 &&
        settings.minSize > 0 &&
        (settings.maxSize < settings.minSize)) {
      throw FindException('Invalid range for minsize and maxsize');
    }
  }

  bool _anyMatchesAnyPattern(Iterable<String> elems, Set<Pattern> patterns) {
    return elems.any((elem) => _matchesAnyPattern(elem, patterns));
  }

  bool _matchesAnyPattern(String s, Set<Pattern> patterns) {
    return patterns.any((p) => p.allMatches(s).isNotEmpty);
  }

  bool isMatchingDir(Directory dir) {
    var elems = path.split(dir.path).where((e) => e.isNotEmpty).toSet();
    if (!settings.includeHidden &&
        elems.any((elem) => FileUtil.isHidden(elem))) {
      return false;
    }
    return (settings.inDirPatterns.isEmpty ||
            _anyMatchesAnyPattern(elems, settings.inDirPatterns)) &&
        (settings.outDirPatterns.isEmpty ||
            !_anyMatchesAnyPattern(elems, settings.outDirPatterns));
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
    var fileName = path.basename(fr.file.path);
    if (settings.inExtensions.isNotEmpty || settings.outExtensions.isNotEmpty) {
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

  Future<FileResult?> filterToFileResult(File f) {
    var fileName = path.basename(f.path);
    if (!settings.includeHidden && FileUtil.isHidden(fileName)) {
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
    return _fileTypes.getFileType(fileName).then((fileType) {
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
    });
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
    List<FileSystemEntity> entries = dir.listSync(recursive: false).toList();
    for (var entry in entries) {
      if (entry is Directory && recurse && isMatchingDir(entry)) {
        pathDirs.add(entry);
      } else if (entry is File && (minDepth < 0 || currentDepth >= minDepth)) {
        pathFiles.add(entry);
      }
    }

    // Filter files
    filterToFileResults(pathFiles).then((pathResults) {
      fileResults.addAll(pathResults);
    });

    // Recurse into dirs
    for (var pathDir in pathDirs) {
      var subFileResults = await _recGetFileResultsForDir(
          pathDir, minDepth, maxDepth, currentDepth + 1);
      fileResults.addAll(subFileResults);
    }
    return fileResults;
  }

  Future<List<FileResult>> _getFileResultsForPath(String filePath) async {
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
      } else if (fsType == FileSystemEntityType.file) {
        // if min_depth > zero, we can skip since the file is at depth zero
        if (settings.minDepth > 0) {
          return [];
        }
        var startFile = File(filePath);
        if (isMatchingDir(startFile.parent)) {
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
      } else {
        throw FindException('Startpath is not a findable file type');
      }
    });
  }

  int cmpByFilePath(FileResult fr1, FileResult fr2) {
    String p1 = fr1.file.parent.path;
    String p2 = fr2.file.parent.path;
    if (settings.sortCaseInsensitive) {
      p1 = p1.toLowerCase();
      p2 = p2.toLowerCase();
    }
    if (p1 == p2) {
      String f1 = path.basename(fr1.file.path);
      String f2 = path.basename(fr2.file.path);
      if (settings.sortCaseInsensitive) {
        f1 = f1.toLowerCase();
        f2 = f2.toLowerCase();
      }
      return f1.compareTo(f2);
    }
    return p1.compareTo(p2);
  }

  int cmpByFileName(FileResult fr1, FileResult fr2) {
    String f1 = path.basename(fr1.file.path);
    String f2 = path.basename(fr2.file.path);
    if (settings.sortCaseInsensitive) {
      f1 = f1.toLowerCase();
      f2 = f2.toLowerCase();
    }
    if (f1 == f2) {
      String p1 = fr1.file.parent.path;
      String p2 = fr2.file.parent.path;
      if (settings.sortCaseInsensitive) {
        p1 = p1.toLowerCase();
        p2 = p2.toLowerCase();
      }
      return p1.compareTo(p2);
    }
    return f1.compareTo(f2);
  }

  int cmpByFileSize(FileResult fr1, FileResult fr2) {
    if (fr1.fileSize == fr2.fileSize) {
      return cmpByFilePath(fr1, fr2);
    }
    return fr1.fileSize.compareTo(fr2.fileSize);
  }

  int cmpByFileType(FileResult fr1, FileResult fr2) {
    if (fr1.fileType != fr2.fileType) {
      return fr1.fileType.index.compareTo(fr2.fileType.index);
    }
    return cmpByFilePath(fr1, fr2);
  }

  int cmpByLastMod(FileResult fr1, FileResult fr2) {
    if (fr1.lastMod == null ||
        fr2.lastMod == null ||
        fr1.lastMod!.isAtSameMomentAs(fr2.lastMod!)) {
      return cmpByFilePath(fr1, fr2);
    }
    return fr1.lastMod!.compareTo(fr2.lastMod!);
  }

  void reverseFileResults(List<FileResult> fileResults) {
    int i = 0;
    int j = fileResults.length - 1;
    while (i < j) {
      var temp = fileResults[i];
      fileResults[i] = fileResults[j];
      fileResults[j] = temp;
      i++;
      j--;
    }
  }

  void sortFileResults(List<FileResult> fileResults) {
    if (settings.sortBy == SortBy.fileName) {
      fileResults.sort(cmpByFileName);
    } else if (settings.sortBy == SortBy.fileSize) {
      fileResults.sort(cmpByFileSize);
    } else if (settings.sortBy == SortBy.fileType) {
      fileResults.sort(cmpByFileType);
    } else if (settings.sortBy == SortBy.lastMod) {
      fileResults.sort(cmpByLastMod);
    } else {
      fileResults.sort(cmpByFilePath);
    }
    if (settings.sortDescending) {
      reverseFileResults(fileResults);
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
      sortFileResults(fileResults);
    }
    return fileResults;
  }

  Future<List<FileResult>> find() async {
    return Future.wait([_fileTypes.ready]).then((res) {
      return _findFiles();
    });
  }
}
