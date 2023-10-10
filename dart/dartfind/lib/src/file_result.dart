import 'dart:io' show File;

import 'package:dartfind/src/file_types.dart';

class FileResult {
  static const String containerSeparator = '!';

  List<String> containers = [];
  final File file;
  final FileType fileType;
  final String? mimeType;
  final int fileSize;
  final DateTime? lastMod;

  FileResult(
      this.file, this.fileType, this.mimeType, this.fileSize, this.lastMod);

  @override
  String toString() {
    var s = '';
    if (containers.isNotEmpty) {
      s = containers.join(containerSeparator) + containerSeparator;
    }
    s += file.path;
    if (mimeType != null) {
      s += ' ($mimeType)';
    }
    return s;
  }
}
