import 'dart:convert' show json;
import 'dart:io' show File;

import 'package:dartfind/src/config.dart' show fileTypesPath;
import 'package:dartfind/src/file_util.dart';

enum FileType {
  unknown,
  archive,
  audio,
  binary,
  code,
  font,
  image,
  text,
  video,
  xml,
}

extension FileTypeExtension on FileType {
  String get name {
    switch (this) {
      case FileType.archive:
        return FileTypes.archive;
      case FileType.audio:
        return FileTypes.audio;
      case FileType.code:
        return FileTypes.code;
      case FileType.font:
        return FileTypes.font;
      case FileType.image:
        return FileTypes.image;
      case FileType.binary:
        return FileTypes.binary;
      case FileType.text:
        return FileTypes.text;
      case FileType.video:
        return FileTypes.video;
      case FileType.xml:
        return FileTypes.xml;
      default:
        return FileTypes.unknown;
    }
  }
}

class FileTypes {
  static const archive = 'archive';
  static const audio = 'audio';
  static const binary = 'binary';
  static const code = 'code';
  static const font = 'font';
  static const image = 'image';
  static const text = 'text';
  static const video = 'video';
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
      case FileTypes.archive:
        return FileType.archive;
      case FileTypes.audio:
        return FileType.audio;
      case FileTypes.binary:
        return FileType.binary;
      case FileTypes.code:
        return FileType.code;
      case FileTypes.font:
        return FileType.font;
      case FileTypes.image:
        return FileType.image;
      case FileTypes.text:
        return FileType.text;
      case FileTypes.video:
        return FileType.video;
      case FileTypes.xml:
        return FileType.xml;
      default:
        return FileType.unknown;
    }
  }

  Future<FileType> getFileType(String fileName) async {
    return await ready.then((_) {
      var ext = FileUtil.extension(fileName);
      // more specific first
      if (fileTypeNameMap[code].contains(fileName) ||
          fileTypeExtMap[code].contains(ext)) {
        return FileType.code;
      }
      if (fileTypeNameMap[archive].contains(fileName) ||
          fileTypeExtMap[archive].contains(ext)) {
        return FileType.archive;
      }
      if (fileTypeNameMap[audio].contains(fileName) ||
          fileTypeExtMap[audio].contains(ext)) {
        return FileType.audio;
      }
      if (fileTypeNameMap[font].contains(fileName) ||
          fileTypeExtMap[font].contains(ext)) {
        return FileType.font;
      }
      if (fileTypeNameMap[image].contains(fileName) ||
          fileTypeExtMap[image].contains(ext)) {
        return FileType.image;
      }
      if (fileTypeNameMap[video].contains(fileName) ||
          fileTypeExtMap[video].contains(ext)) {
        return FileType.video;
      }
      // more general last
      if (fileTypeNameMap[xml].contains(fileName) ||
          fileTypeExtMap[xml].contains(ext)) {
        return FileType.xml;
      }
      if (fileTypeNameMap[text].contains(fileName) ||
          fileTypeExtMap[text].contains(ext)) {
        return FileType.text;
      }
      if (fileTypeNameMap[binary].contains(fileName) ||
          fileTypeExtMap[binary].contains(ext)) {
        return FileType.binary;
      }
      return FileType.unknown;
    });
  }

  Future<bool> isArchiveFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeNameMap[archive].contains(fileName) ||
          fileTypeExtMap[archive].contains(FileUtil.extension(fileName));
    });
  }

  Future<bool> isAudioFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeNameMap[audio].contains(fileName) ||
          fileTypeExtMap[audio].contains(FileUtil.extension(fileName));
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

  Future<bool> isFontFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeNameMap[font].contains(fileName) ||
          fileTypeExtMap[font].contains(FileUtil.extension(fileName));
    });
  }

  Future<bool> isImageFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeNameMap[image].contains(fileName) ||
          fileTypeExtMap[image].contains(FileUtil.extension(fileName));
    });
  }

  Future<bool> isTextFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeNameMap[text].contains(fileName) ||
          fileTypeExtMap[text].contains(FileUtil.extension(fileName));
    });
  }

  Future<bool> isVideoFile(String fileName) async {
    return await ready.then((_) {
      return fileTypeNameMap[video].contains(fileName) ||
          fileTypeExtMap[video].contains(FileUtil.extension(fileName));
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
