import 'dart:async';
import 'dart:io';
import 'dart:convert';
import 'dart:math';

import 'package:dartfind/src/common.dart';
import 'package:dartfind/src/file_types.dart';
import 'package:dartfind/src/file_util.dart';
import 'package:dartfind/src/find_exception.dart';
import 'package:dartfind/src/find_file.dart';
import 'package:dartfind/src/find_result.dart';
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
  Encoding _encoding;
  Encoding _binaryEncoding;

  Finder(this.settings) {
    _fileTypes = FileTypes();
    validated = _validateSettings();
    _encoding = systemEncoding;
    _binaryEncoding = Latin1Codec(allowInvalid: true);
  }

  Future<void> _validateSettings() async {
    if (settings.startPath == null || settings.startPath.isEmpty) {
      throw FindException('Startpath not defined');
    }
    var startPath = FileUtil.expandPath(settings.startPath);
    if (await FileSystemEntity.type(startPath) == FileSystemEntityType.notFound) {
      throw FindException('Startpath not found');
    }
    if (settings.findPatterns.isEmpty) {
      throw FindException('No find patterns defined');
    }
    if (settings.linesAfter < 0) {
      throw FindException('Invalid linesafter');
    }
    if (settings.linesBefore < 0) {
      throw FindException('Invalid linesbefore');
    }
    if (settings.maxLineLength < 0) {
      throw FindException('Invalid maxlinelength');
    }
    _encoding = Encoding.getByName(settings.textFileEncoding);
    if (_encoding == null) {
      throw FindException('Invalid or unsupported encoding: ${settings.textFileEncoding}');
    } else if (allowInvalid) {
      if (_encoding == utf8) {
        _encoding = Utf8Codec(allowMalformed: true);
      } else if (_encoding == latin1) {
        _encoding = Latin1Codec(allowInvalid: true);
      } else if (_encoding == ascii) {
        _encoding = AsciiCodec(allowInvalid: true);
      }
    }
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

  Future<List<FindResult>> _findBinaryFile(FindFile sf) async {
    if (settings.debug) {
      log('Finding binary file $sf');
    }
    return sf.file.readAsString(encoding: _binaryEncoding).then((String contents) {
      var results = <FindResult>[];
      for (var p in settings.findPatterns) {
        var matches = [];
        if (settings.firstMatch) {
          var match = (p as RegExp).firstMatch(contents);
          if (match != null) {
            matches = [match];
          }
        } else {
          matches = p.allMatches(contents).toList();
        }
        for (var m in matches) {
          results.add(FindResult(p, sf, 0, m.start + 1, m.end + 1, null, [], []));
        }
      }
      return results;
    });
  }

  bool _linesMatch(List<String> lines, Set<Pattern> inPatterns, Set<Pattern> outPatterns) {
    return lines.isEmpty ||
        ((inPatterns.isEmpty || _anyMatchesAnyPattern(lines, inPatterns))
            &&
            (outPatterns.isEmpty || !_anyMatchesAnyPattern(lines, outPatterns)));
  }

  Future<List<FindResult>> findLineStream(Stream<String> stream) async {
    var results = <FindResult>[];
    var lineNum = 0;
    var matchedPatterns = {};
    if (settings.firstMatch) {
      stream = stream.takeWhile((_) {
        return matchedPatterns.length < settings.findPatterns.length;
      });
    }

    try {
      var linesBefore = <String>[];
      var linesAfter = <String>[];

      var it = StreamIterator(stream);

      while (true) {
        lineNum++;
        var line;
        if (linesAfter.isNotEmpty) {
          line = linesAfter.removeAt(0);
        } else if (await it.moveNext()) {
          line = it.current;
        } else {
          break;
        }
        if (settings.linesAfter > 0) {
          while (linesAfter.length < settings.linesAfter && await it.moveNext()) {
            linesAfter.add(it.current);
          }
        }

        var findPatterns = <Pattern>{};
        if (settings.firstMatch) {
          findPatterns = settings.findPatterns
              .where((elem) => !matchedPatterns.containsKey(elem)).toSet();
        } else {
          findPatterns = settings.findPatterns;
        }
        if (findPatterns.isEmpty) {
          break;
        }
        for (var p in findPatterns) {
          var matches = [];
          if (settings.firstMatch) {
            var match = (p as RegExp).firstMatch(line);
            if (match != null) {
              matches = [match];
              matchedPatterns[p] = 1;
            }
          } else {
            matches = p.allMatches(line).toList();
          }

          if (matches.isEmpty
              || !_linesMatch(linesBefore, settings.inLinesBeforePatterns,
                  settings.outLinesBeforePatterns)
              || !_linesMatch(linesAfter, settings.inLinesAfterPatterns,
                  settings.outLinesAfterPatterns)) {
            continue;
          }

          for (var m in matches) {
            results.add(FindResult(p, null, lineNum, m.start + 1, m.end + 1,
                line, linesBefore.toList(), linesAfter.toList()));
          }
        }

        if (settings.linesBefore > 0) {
          if (linesBefore.length == settings.linesBefore) {
            linesBefore.removeAt(0);
          }
          if (linesBefore.length < settings.linesBefore) {
            linesBefore.add(line);
          }
        }
        if (settings.firstMatch && matchedPatterns.length == settings.findPatterns.length) {
          break;
        }
      }

      return results;

    } catch (e) {
      log(e.toString());
      return [];
    }
  }

  Future<List<FindResult>> _findTextFileLines(FindFile sf) async {
    var results = <FindResult>[];
    var inputStream = sf.file.openRead();
    try {
      var lineStream =
        _encoding.decoder.bind(inputStream).transform(LineSplitter());
      return findLineStream(lineStream).then((results) {
        return results.map((r) {
          r.file = sf;
          return r;
        }).toList();
      });
    } on FormatException catch(e) {
      log('Error reading ${sf.file}: ${e.message}');
    } catch (e) {
      log(e.toString());
    }
    return results;
  }

  List<String> _getLinesFromMultilineString(String s, List<int> newLineIndices) {
    var lines = <String>[];
    for (var i=0; i < newLineIndices.length - 1; ++i) {
      lines.add(s.substring(newLineIndices[i] + 1, newLineIndices[i+1]));
    }
    return lines;
  }

  List<FindResult> findMultilineString(String s) {
    var results = <FindResult>[];
    var newLineIndices = <int>[];
    var it = s.runes.iterator;
    var i = 0;
    while (it.moveNext()) {
      if (it.current == 10) {
        newLineIndices.add(i);
      }
      i++;
    }

    for (var p in settings.findPatterns) {
      var matches = [];
      if (settings.firstMatch) {
        var match = (p as RegExp).firstMatch(s);
        if (match != null) {
          matches = [match];
        }
      } else {
        matches = p.allMatches(s).toList();
      }
      for (var m in matches) {
        var beforeNewlineIndices =
          newLineIndices.takeWhile((i) => i <= m.start).toList();
        var linesBefore = <String>[];
        if (settings.linesBefore > 0) {
          var linesBeforeIndices = beforeNewlineIndices.reversed
              .take(settings.linesBefore + 1).toList().reversed.toList();
          linesBefore = _getLinesFromMultilineString(s, linesBeforeIndices);
        }
        var afterNewlineIndices =
          newLineIndices.skipWhile((i) => i <= m.start).toList();
        var linesAfter = <String>[];
        if (settings.linesAfter > 0) {
          var linesAfterIndices =
            afterNewlineIndices.take(settings.linesAfter + 1).toList();
          linesAfter = _getLinesFromMultilineString(s, linesAfterIndices);
        }

        if (!_linesMatch(linesBefore, settings.inLinesBeforePatterns,
                settings.outLinesBeforePatterns)
            || !_linesMatch(linesAfter, settings.inLinesAfterPatterns,
                settings.outLinesAfterPatterns)) {
          continue;
        }

        var lineNum = beforeNewlineIndices.length + 1;
        var startLineIndex = 0;
        if (lineNum > 1) {
          startLineIndex = beforeNewlineIndices.last + 1;
        }
        var endLineIndex = s.length - 1;
        if (afterNewlineIndices.isNotEmpty) {
          endLineIndex = afterNewlineIndices.first;
        }
        var line = s.substring(startLineIndex, endLineIndex);
        var startMatchIndex = m.start - startLineIndex + 1;
        var endMatchIndex = m.end - startLineIndex + 1;
        results.add(FindResult(p, null, lineNum, startMatchIndex,
            endMatchIndex, line, linesBefore, linesAfter));
      }
    }

    return results;
  }

  Future<List<FindResult>> _findTextFileContents(FindFile sf) async {
    return sf.file.readAsString(encoding: _encoding).then((String contents) {
      var results = findMultilineString(contents);
      return results.map((r) {
        r.file = sf;
        return r;
      }).toList();
    });
  }

  Future<List<FindResult>> _findTextFile(FindFile sf) async {
    if (settings.debug) {
      log('Finding text file $sf');
    }
    if (settings.multiLineFind) {
      return _findTextFileContents(sf);
    } else {
      return _findTextFileLines(sf);
    }
  }

  Future<List<FindResult>> _findFile(FindFile sf) async {
    var results = <FindResult>[];
    if ({FileType.text, FileType.code, FileType.xml}.contains(sf.fileType)) {
      return _findTextFile(sf);
    } else if (sf.fileType == FileType.binary) {
      return _findBinaryFile(sf);
    }
    return results;
  }

  Future<List<FindResult>> _findFiles(List<FindFile> findFiles) async {
    if (settings.verbose) {
      findFiles.sort((sf1, sf2) {
        if (sf1.file.parent.path == sf2.file.parent.path) {
          return sf1.file.path.compareTo(sf2.file.path);
        }
        return sf1.file.parent.path.compareTo(sf2.file.parent.path);
      });
      var findDirs = findFiles.map((sf) => sf.file.parent.path).toSet().toList();
      log('\nDirectories to be found (${findDirs.length}):');
      findDirs.forEach((d) => log(FileUtil.contractPath(d)));
      log('\nFiles to be found (${findFiles.length}):');
      findFiles.forEach((sf) => log(FileUtil.contractPath(sf.file.path)));
    }
    // this is the (almost) largest batch size you can have before you get the
    // "too many files open" errors
    var _batchSize = 245;
    var _offset = 0;

    var results = <FindResult>[];

    while (_offset < findFiles.length) {
      var toIndex = min(_offset + _batchSize, findFiles.length);
      var fileResultsFutures = findFiles.sublist(_offset, toIndex)
          .map((sf) => _findFile(sf));
      await Future.wait(fileResultsFutures).then((filesResults) {
        for (var fileResults in filesResults) {
          results.addAll(fileResults);
        }
      });
      _offset += _batchSize;
    }

    return results;
  }

  Future<FindFile> filterToFindFile(File f) {
    if (settings.excludeHidden && FileUtil.isHidden(path.basename(f.path))) {
      return null;
    }
    return _fileTypes.getFileType(f.path).then((fileType) {
      var findFile = FindFile(f, fileType);
      if ((findFile.fileType == FileType.archive && settings.findArchives)
          || (!settings.archivesOnly && isFindFile(findFile))) {
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

  Future<List<FindResult>> find() async {
    return Future.wait([_fileTypes.ready, validated]).then((res) {
      return _getFindFiles(FileUtil.expandPath(settings.startPath))
          .then((findFiles) => _findFiles(findFiles));
    });
  }
}
