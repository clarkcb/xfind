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
      var startPath = FileUtil.expandPath(p);
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
    if (settings.excludeHidden &&
        elems.any((elem) => FileUtil.isHidden(elem))) {
      return false;
    }
    return (settings.inDirPatterns.isEmpty ||
            _anyMatchesAnyPattern(elems, settings.inDirPatterns)) &&
        (settings.outDirPatterns.isEmpty ||
            !_anyMatchesAnyPattern(elems, settings.outDirPatterns));
  }

  bool isMatchingFileResult(FileResult fr) {
    var fileName = path.basename(fr.file.path);
    if (settings.inExtensions.isNotEmpty || settings.outExtensions.isNotEmpty) {
      var ext = FileUtil.extension(fileName);
      if ((settings.inExtensions.isNotEmpty &&
              !settings.inExtensions.contains(ext)) ||
          (settings.outExtensions.isNotEmpty &&
              settings.outExtensions.contains(ext))) {
        return false;
      }
    }
    if ((settings.inFilePatterns.isNotEmpty &&
            !_matchesAnyPattern(fileName, settings.inFilePatterns)) ||
        (settings.outFilePatterns.isNotEmpty &&
            _matchesAnyPattern(fileName, settings.outFilePatterns))) {
      return false;
    }
    if ((settings.inFileTypes.isNotEmpty &&
            !settings.inFileTypes.contains(fr.fileType)) ||
        (settings.outFileTypes.isNotEmpty &&
            settings.outFileTypes.contains(fr.fileType))) {
      return false;
    }
    if (fr.stat != null) {
      if ((settings.maxLastMod != null &&
              fr.stat!.changed.isAfter(settings.maxLastMod!)) ||
          (settings.minLastMod != null &&
              fr.stat!.changed.isBefore(settings.minLastMod!))) {
        return false;
      }
      if ((settings.maxSize > 0 && fr.stat!.size > settings.maxSize) ||
          (settings.minSize > 0 && fr.stat!.size < settings.minSize)) {
        return false;
      }
    }
    return true;
  }

  bool isMatchingArchiveFile(FileResult sf) {
    var fileName = sf.file.path.split(Platform.pathSeparator).last;
    if (settings.excludeHidden && FileUtil.isHidden(fileName)) {
      return false;
    }
    var ext = FileUtil.extension(fileName);
    return (settings.inArchiveExtensions.isEmpty ||
            settings.inArchiveExtensions.contains(ext)) &&
        (settings.outArchiveExtensions.isEmpty ||
            !settings.outArchiveExtensions.contains(ext)) &&
        (settings.inArchiveFilePatterns.isEmpty ||
            _matchesAnyPattern(fileName, settings.inArchiveFilePatterns)) &&
        (settings.outArchiveFilePatterns.isEmpty ||
            !_matchesAnyPattern(fileName, settings.outArchiveFilePatterns));
  }

  Future<FileResult?> filterToFileResult(File f) {
    var fileName = path.basename(f.path);
    if (settings.excludeHidden && FileUtil.isHidden(fileName)) {
      return Future.value(null);
    }
    FileStat? stat;
    if (settings.needStat()) {
      stat = FileStat.statSync(f.path);
    }
    return _fileTypes.getFileType(fileName).then((fileType) {
      var fileResult = FileResult(f, fileType, stat);
      if (fileResult.fileType == FileType.archive) {
        if (settings.includeArchives) {
          return fileResult;
        }
        return Future.value(null);
      }
      if (!settings.archivesOnly && isMatchingFileResult(fileResult)) {
        return fileResult;
      }
      return Future.value(null);
    });
  }

  Future<List<FileResult>> _getFileResultsForPath(String startPath) async {
    var isDir = FileSystemEntity.isDirectory(startPath);
    var isFile = FileSystemEntity.isFile(startPath);
    return Future.wait([isDir, isFile]).then((res) {
      var fileResults = <FileResult>[];
      var startPathSepCount = FileUtil.sepCount(startPath);
      if (res.first) {
        // if max_depth is zero, we can skip since a directory cannot be a result
        if (settings.maxDepth == 0) {
          return [];
        }
        var dir = Directory(startPath);
        return dir.list(recursive: settings.recursive).listen((f) async {
          var fileSepCount = FileUtil.sepCount(f.path);
          var depth = fileSepCount - startPathSepCount;
          if (f is File &&
              depth >= settings.minDepth &&
              depth <= settings.maxDepth &&
              isMatchingDir(f.parent)) {
            var fileResult = await filterToFileResult(f);
            if (fileResult != null) {
              fileResults.add(fileResult);
            }
          }
        }).asFuture(fileResults);
      } else if (res.last) {
        // if min_depth > zero, we can skip since the file is at depth zero
        if (settings.minDepth > 0) {
          return [];
        }
        var startFile = File(startPath);
        if (isMatchingDir(startFile.parent)) {
          FileStat? stat;
          if (settings.needStat()) {
            stat = FileStat.statSync(startPath);
          }
          return _fileTypes.getFileType(startPath).then((fileType) {
            var fileResult = FileResult(startFile, fileType, stat);
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
    if (fr1.stat != null &&
        fr2.stat != null &&
        fr1.stat!.size != fr2.stat!.size) {
      return fr1.stat!.size < fr2.stat!.size ? -1 : 1;
    }
    return cmpByFilePath(fr1, fr2);
  }

  int cmpByFileType(FileResult fr1, FileResult fr2) {
    if (fr1.fileType != fr2.fileType) {
      return fr1.fileType.index.compareTo(fr2.fileType.index);
    }
    return cmpByFilePath(fr1, fr2);
  }

  int cmpByLastMod(FileResult fr1, FileResult fr2) {
    if (fr1.stat != null &&
        fr2.stat != null &&
        !fr1.stat!.modified.isAtSameMomentAs(fr2.stat!.modified)) {
      return fr1.stat!.modified.compareTo(fr2.stat!.modified);
    }
    return cmpByFilePath(fr1, fr2);
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
        settings.paths.map((p) => _getFileResultsForPath(p));
    await Future.wait(pathsFileResultsFutures).then((pathsFileResults) {
      for (var pathFileResults in pathsFileResults) {
        fileResults.addAll(pathFileResults);
      }
    });
    sortFileResults(fileResults);
    return fileResults;
  }

  Future<List<FileResult>> find() async {
    return Future.wait([_fileTypes.ready]).then((res) {
      return _findFiles();
    });
  }
}
