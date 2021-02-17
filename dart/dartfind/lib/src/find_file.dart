import 'dart:io' show File;

import 'package:dartfind/src/file_types.dart';

class FindFile {
  static const String CONTAINER_SEPARATOR = '!';

  List<String> containers = [];
  final File file;
  final FileType fileType;

  FindFile(this.file, this.fileType);

  @override
  String toString() {
    var s ='';
    if (containers.isNotEmpty) {
      s = containers.join(CONTAINER_SEPARATOR) + CONTAINER_SEPARATOR;
    }
    s += file.path;
    return s;
  }
}
