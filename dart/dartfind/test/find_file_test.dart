import 'dart:io' show File, Platform;

import 'package:dartfind/dartfind.dart' show FileType, FindFile;
import 'package:test/test.dart';

void main() {
  test('test FindFile with absolute path', () {
    var path = Platform.environment['HOME'] + '/src/xfind/dart/dartfind/lib/src/find_file.dart';
    var findFile = FindFile(File(path), FileType.code);
    expect(findFile.toString(), path);
  });

  test('test FindFile with tilde path', () {
    var path = '~/src/xfind/dart/dartfind/lib/src/find_file.dart';
    var findFile = FindFile(File(path), FileType.code);
    expect(findFile.toString(), path);
  });

  test('test FindFile with rel path #1', () {
    var path = './find_file.dart';
    var findFile = FindFile(File(path), FileType.code);
    expect(findFile.toString(), path);
  });

  test('test FindFile with rel path #2', () {
    var path = './find_file.dart';
    var findFile = FindFile(File(path), FileType.code);
    expect(findFile.toString(), path);
  });
}
