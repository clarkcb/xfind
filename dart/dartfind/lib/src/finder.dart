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
  FileTypes _fileTypes;
  Future validated;
  // whether to allow invalid/malformed characters in encoding/decoding
  // setting to false means find will end once an attempt is made to read
  // a file in an incompatible encoding
  final bool allowInvalid = true;
  Finder(this.settings) {
    _fileTypes = FileTypes();
    validated = _validateSettings();
  }

  Future<void> _validateSettings() async {
    if (settings.paths.isEmpty) {
      throw FindException('Startpath not defined');
    }
    settings.paths.forEach((p) async {
      var startPath = FileUtil.expandPath(p);
      if (await FileSystemEntity.type(startPath) ==
          FileSystemEntityType.notFound) {
        throw FindException('Startpath not found');
      }
    });
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

  bool isMatchingFile(FileResult sf) {
    var fileName = path.basename(sf.file.path);
    var ext = FileUtil.extension(fileName);
    return (settings.inExtensions.isEmpty ||
            settings.inExtensions.contains(ext)) &&
        (settings.outExtensions.isEmpty ||
            !settings.outExtensions.contains(ext)) &&
        (settings.inFilePatterns.isEmpty ||
            _matchesAnyPattern(fileName, settings.inFilePatterns)) &&
        (settings.outFilePatterns.isEmpty ||
            !_matchesAnyPattern(fileName, settings.outFilePatterns)) &&
        (settings.inFileTypes.isEmpty ||
            settings.inFileTypes.contains(sf.fileType)) &&
        (settings.outFileTypes.isEmpty ||
            !settings.outFileTypes.contains(sf.fileType));
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

  Future<FileResult> filterToFileResult(File f) {
    var fileName = path.basename(f.path);
    if (settings.excludeHidden && FileUtil.isHidden(fileName)) {
      return null;
    }
    return _fileTypes.getFileType(fileName).then((fileType) {
      var findFile = FileResult(f, fileType);
      if (findFile.fileType == FileType.archive) {
        if (settings.includeArchives) {
          return findFile;
        }
        return null;
      }
      if (!settings.archivesOnly && isMatchingFile(findFile)) {
        return findFile;
      }
      return null;
    });
  }

  Future<List<FileResult>> _getFileResultsForPath(String startPath) async {
    var isDir = FileSystemEntity.isDirectory(startPath);
    var isFile = FileSystemEntity.isFile(startPath);
    return Future.wait([isDir, isFile]).then((res) {
      var fileResults = <FileResult>[];
      if (res.first) {
        var dir = Directory(startPath);
        return dir.list(recursive: settings.recursive).listen((f) async {
          if (f is File && isMatchingDir(f.parent)) {
            var fileResult = await filterToFileResult(f);
            if (fileResult != null) {
              fileResults.add(fileResult);
            }
          }
        }).asFuture(fileResults);
      } else if (res.last) {
        var startFile = File(startPath);
        if (isMatchingDir(startFile.parent)) {
          return _fileTypes.getFileType(startPath).then((fileType) {
            var fileResult = FileResult(startFile, fileType);
            if (isMatchingFile(fileResult)) {
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

  Future<List<FileResult>> _findFiles() async {
    var fileResults = <FileResult>[];
    var pathsFileResultsFutures =
        settings.paths.map((p) => _getFileResultsForPath(p));
    await Future.wait(pathsFileResultsFutures).then((pathsFileResults) {
      for (var pathFileResults in pathsFileResults) {
        fileResults.addAll(pathFileResults);
      }
    });
    return fileResults;
  }

  Future<List<FileResult>> find() async {
    return Future.wait([_fileTypes.ready, validated]).then((res) {
      return _findFiles();
    });
  }
}
