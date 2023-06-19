import 'dart:convert' show json;
import 'dart:io' show File;

import 'package:dartfind/src/config.dart' show fileTypesPath;
import 'package:dartfind/src/file_util.dart';

enum FileType {
  unknown,
  archive,
  binary,
  code,
  text,
  xml,
}

extension FileTypeExtension on FileType {
  String get name {
    switch (this) {
      case FileType.text:
        return FileTypes.text;
      case FileType.code:
        return FileTypes.code;
      case FileType.xml:
        return FileTypes.xml;
      case FileType.binary:
        return FileTypes.binary;
      case FileType.archive:
        return FileTypes.archive;
      default:
        return FileTypes.unknown;
    }
  }
}

class FileTypes {
  static const archive = 'archive';
  static const binary = 'binary';
  static const code = 'code';
  static const text = 'text';
  static const xml = 'xml';
  static const unknown = 'unknown';

  var fileTypeExtMap = {};
  var fileTypeNameMap = {};
  late Future ready;

  FileTypes() {
    ready = loadFileTypesFromJson();
  }

  Future<void> loadFileTypesFromJson() async {
    var contents = await File(fileTypesPath).readAsString();
    Map jsonFileTypesMap = json.decode(contents);
    if (jsonFileTypesMap.containsKey('filetypes')) {
      var ftList = jsonFileTypesMap['filetypes'] as List;
      for (var ft in ftList) {
        var typeName = (ft as Map)['type'];
        var extensions = (ft)['extensions'].toSet();
        fileTypeExtMap[typeName] = extensions;
        var names = (ft)['names'].toSet();
        fileTypeNameMap[typeName] = names;
      }
      fileTypeExtMap[text] = fileTypeExtMap[text]
          .union(fileTypeExtMap[code].union(fileTypeExtMap[xml]));
      fileTypeNameMap[text] = fileTypeNameMap[text]
          .union(fileTypeNameMap[code].union(fileTypeNameMap[xml]));
    }
  }

  static FileType fromName(String typeName) {
    switch (typeName.trim().toLowerCase()) {
      case FileTypes.text:
        return FileType.text;
      case FileTypes.code:
        return FileType.code;
      case FileTypes.xml:
        return FileType.xml;
      case FileTypes.binary:
        return FileType.binary;
      case FileTypes.archive:
        return FileType.archive;
      default:
        return FileType.unknown;
    }
  }

  Future<FileType> getFileType(String fileName) async {
    return await ready.then((_) {
      var ext = FileUtil.extension(fileName);
      if (fileTypeNameMap[code].contains(fileName) ||
          fileTypeExtMap[code].contains(ext)) return FileType.code;
      if (fileTypeNameMap[xml].contains(fileName) ||
          fileTypeExtMap[xml].contains(ext)) return FileType.xml;
      if (fileTypeNameMap[text].contains(fileName) ||
          fileTypeExtMap[text].contains(ext)) return FileType.text;
      if (fileTypeNameMap[binary].contains(fileName) ||
          fileTypeExtMap[binary].contains(ext)) return FileType.binary;
      if (fileTypeNameMap[archive].contains(fileName) ||
          fileTypeExtMap[archive].contains(ext)) return FileType.archive;
      return FileType.unknown;
    });
  }

  Future<bool> isArchiveFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeNameMap[archive].contains(fileName) ||
          fileTypeExtMap[archive].contains(FileUtil.extension(fileName));
    });
  }

  Future<bool> isBinaryFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeNameMap[binary].contains(fileName) ||
          fileTypeExtMap[binary].contains(FileUtil.extension(fileName));
    });
  }

  Future<bool> isCodeFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeNameMap[code].contains(fileName) ||
          fileTypeExtMap[code].contains(FileUtil.extension(fileName));
    });
  }

  Future<bool> isTextFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeNameMap[text].contains(fileName) ||
          fileTypeExtMap[text].contains(FileUtil.extension(fileName));
    });
  }

  Future<bool> isXmlFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeNameMap[xml].contains(fileName) ||
          fileTypeExtMap[xml].contains(FileUtil.extension(fileName));
    });
  }

  Future<bool> isUnknownFile(String fileName) async {
    return await ready.then((_) async {
      var fileType = await getFileType(fileName);
      return fileType == FileType.unknown;
    });
  }
}
