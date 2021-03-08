import 'dart:async';
import 'dart:io';

import 'package:dartfind/src/file_types.dart';
import 'package:dartfind/src/file_util.dart';
import 'package:dartfind/src/find_exception.dart';
import 'package:dartfind/src/find_file.dart';
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
      if (await FileSystemEntity.type(startPath) == FileSystemEntityType.notFound) {
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

  bool isFindDir(Directory dir) {
    var elems = path.split(dir.path).where((e) => e.isNotEmpty).toSet();
    if (settings.excludeHidden && elems.any((elem) => FileUtil.isHidden(elem))) {
      return false;
    }
    return (settings.inDirPatterns.isEmpty
        || _anyMatchesAnyPattern(elems, settings.inDirPatterns))
        && (settings.outDirPatterns.isEmpty
            || !_anyMatchesAnyPattern(elems, settings.outDirPatterns));
  }

  bool isFindFile(FindFile sf) {
    var fileName = path.basename(sf.file.path);
    var ext = FileUtil.extension(fileName);
    return (settings.inExtensions.isEmpty
        || settings.inExtensions.contains(ext))
        && (settings.outExtensions.isEmpty
        || !settings.outExtensions.contains(ext))
        && (settings.inFilePatterns.isEmpty
        || _matchesAnyPattern(fileName, settings.inFilePatterns))
        && (settings.outFilePatterns.isEmpty
            || !_matchesAnyPattern(fileName, settings.outFilePatterns))
        && (settings.inFileTypes.isEmpty
            || settings.inFileTypes.contains(sf.fileType))
        && (settings.outFileTypes.isEmpty
            || !settings.outFileTypes.contains(sf.fileType));
  }

  bool isArchiveFindFile(FindFile sf) {
    var fileName = sf.file.path.split(Platform.pathSeparator).last;
    if (settings.excludeHidden && FileUtil.isHidden(fileName)) {
      return false;
    }
    var ext = FileUtil.extension(fileName);
    return (settings.inArchiveExtensions.isEmpty
        || settings.inArchiveExtensions.contains(ext))
        && (settings.outArchiveExtensions.isEmpty
        || !settings.outArchiveExtensions.contains(ext))
        && (settings.inArchiveFilePatterns.isEmpty
        || _matchesAnyPattern(fileName, settings.inArchiveFilePatterns))
        && (settings.outArchiveFilePatterns.isEmpty
            || !_matchesAnyPattern(fileName, settings.outArchiveFilePatterns));
  }

  Future<FindFile> filterToFindFile(File f) {
    if (settings.excludeHidden && FileUtil.isHidden(path.basename(f.path))) {
      return null;
    }
    return _fileTypes.getFileType(f.path).then((fileType) {
      var findFile = FindFile(f, fileType);
      if (findFile.fileType == FileType.archive) {
        if (settings.includeArchives) {
          return findFile;
        }
        return null;
      }
      if (!settings.archivesOnly && isFindFile(findFile)) {
        return findFile;
      }
      return null;
    });
  }

  Future<List<FindFile>> _getFindFiles(String startPath) async {
    var isDir = FileSystemEntity.isDirectory(startPath);
    var isFile = FileSystemEntity.isFile(startPath);
    return Future.wait([isDir, isFile]).then((res) {
      var findFiles = <FindFile>[];
      if (res.first) {
        var dir = Directory(startPath);
        return dir.list(recursive: settings.recursive).listen((f) async {
          if (f is File && isFindDir(f.parent)) {
            var findFile = await filterToFindFile(f);
            if (findFile != null) {
              findFiles.add(findFile);
            }
          }
        }).asFuture(findFiles);
      } else if (res.last) {
        var startFile = File(startPath);
        if (isFindDir(startFile.parent)) {
          return _fileTypes.getFileType(startPath).then((fileType) {
            var findFile = FindFile(startFile, fileType);
            if (isFindFile(findFile)) {
              return [findFile];
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

  Future<List<FindFile>> _findFiles() async {
      var findFiles = <FindFile>[];
      var pathsFindFilesFutures = settings.paths.map((p) => _getFindFiles(p));
      await Future.wait(pathsFindFilesFutures).then((pathsFindFiles) {
        for (var pathFindFiles in pathsFindFiles) {
          findFiles.addAll(pathFindFiles);
        }
      });
      return findFiles;
  }

  Future<List<FindFile>> find() async {
    return Future.wait([_fileTypes.ready, validated]).then((res) {
      return _findFiles();
    });
  }
}
