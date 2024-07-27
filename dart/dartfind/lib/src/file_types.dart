import 'package:sqlite3/sqlite3.dart';

import 'package:dartfind/src/config.dart' show xfindDb;
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

  var extFileTypeMap = {};
  late Future ready;

  var fileTypes = [
    FileType.unknown,
    FileType.archive,
    FileType.audio,
    FileType.binary,
    FileType.code,
    FileType.font,
    FileType.image,
    FileType.text,
    FileType.video,
    FileType.xml,
  ];
  Database? db;

  FileTypes() {
    ready = getDatabase();
  }

  Future<void> getDatabase() async {
    db = sqlite3.open(xfindDb, mode: OpenMode.readOnly);
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

  FileType getFileTypeForQueryAndElem(String query, String elem) {
    final ResultSet result = db!.select(query, [elem]);
    if (result.isEmpty) {
      return FileType.unknown;
    }
    final fileTypeId = result.first['file_type_id'] - 1;
    return fileTypes[fileTypeId];
  }

  FileType getFileTypeForFileName(String fileName) {
    final String query = 'SELECT file_type_id FROM file_name WHERE name = ?';
    return getFileTypeForQueryAndElem(query, fileName);
  }

  FileType getFileTypeForExtension(String fileExt) {
    if (extFileTypeMap.containsKey(fileExt)) {
      return extFileTypeMap[fileExt];
    }
    final String query =
        'SELECT file_type_id FROM file_extension WHERE extension = ?';
    FileType fileType = getFileTypeForQueryAndElem(query, fileExt);
    extFileTypeMap[fileExt] = fileType;
    return fileType;
  }

  Future<FileType> getFileType(String fileName) async {
    return await ready.then((_) {
      final fileTypeForFileName = getFileTypeForFileName(fileName);
      if (fileTypeForFileName != FileType.unknown) {
        return fileTypeForFileName;
      }
      return getFileTypeForExtension(FileUtil.extension(fileName));
    });
  }

  Future<bool> isArchiveFile(String fileName) async {
    return await getFileType(fileName) == FileType.archive;
  }

  Future<bool> isAudioFile(String fileName) async {
    return await getFileType(fileName) == FileType.audio;
  }

  Future<bool> isBinaryFile(String fileName) async {
    return await getFileType(fileName) == FileType.binary;
  }

  Future<bool> isCodeFile(String fileName) async {
    return await getFileType(fileName) == FileType.code;
  }

  Future<bool> isFontFile(String fileName) async {
    return await getFileType(fileName) == FileType.font;
  }

  Future<bool> isImageFile(String fileName) async {
    return await getFileType(fileName) == FileType.image;
  }

  Future<bool> isTextFile(String fileName) async {
    var fileType = await getFileType(fileName);
    return fileType == FileType.text ||
        fileType == FileType.code ||
        fileType == FileType.xml;
  }

  Future<bool> isVideoFile(String fileName) async {
    return await getFileType(fileName) == FileType.video;
  }

  Future<bool> isXmlFile(String fileName) async {
    return await getFileType(fileName) == FileType.xml;
  }

  Future<bool> isUnknownFile(String fileName) async {
    return await getFileType(fileName) == FileType.unknown;
  }
}
