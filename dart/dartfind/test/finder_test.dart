import 'dart:io';

import 'package:dartfind/dartfind.dart';
import 'package:test/test.dart';

FindSettings getSettings() {
  var settings = FindSettings();
  settings.paths.add('.');
  return settings;
}

void main() {
  /***************************************************************************
   * isMatchingDir tests
   **************************************************************************/
  group('isMatchingDir tests', () {
    test('test isFindDir single dot', () {
      var config = FindConfig();
      var settings = getSettings();
      var finder = Finder(config, settings);
      expect(finder.isMatchingDir(Directory('.')), true);
    });

    test('test isMatchingDir double dot', () {
      var config = FindConfig();
      var settings = getSettings();
      var finder = Finder(config, settings);
      expect(finder.isMatchingDir(Directory('..')), true);
    });

    test('test isMatchingDir hidden dir', () {
      var config = FindConfig();
      var settings = getSettings();
      var finder = Finder(config, settings);
      expect(finder.isMatchingDir(Directory('.git')), false);
    });

    test('test isMatchingDir hidden dir includeHidden', () {
      var config = FindConfig();
      var settings = getSettings();
      settings.includeHidden = true;
      var finder = Finder(config, settings);
      expect(finder.isMatchingDir(Directory('.git')), true);
    });

    test('test isMatchingDir no patterns', () {
      var config = FindConfig();
      var settings = getSettings();
      settings.includeHidden = false;
      var finder = Finder(config, settings);
      expect(finder.isMatchingDir(Directory('/Users')), true);
    });

    test('test isMatchingDir dir matches inDirPatterns', () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addPattern('Find', settings.inDirPatterns);
      var finder = Finder(config, settings);
      expect(finder.isMatchingDir(Directory('./CsFind')), true);
    });

    test('test isMatchingDir dir does not match inDirPatterns', () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addPattern('FindFiles', settings.inDirPatterns);
      var finder = Finder(config, settings);
      expect(finder.isMatchingDir(Directory('./CsFind')), false);
    });

    test('test isMatchingDir dir matches outDirPatterns', () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addPattern('Find', settings.outDirPatterns);
      var finder = Finder(config, settings);
      expect(finder.isMatchingDir(Directory('./CsFind')), false);
    });

    test('test isMatchingDir dir does not match outDirPatterns', () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addPattern('FindFiles', settings.outDirPatterns);
      var finder = Finder(config, settings);
      expect(finder.isMatchingDir(Directory('./CsFind')), true);
    });
  });

  /***************************************************************************
   * isMatchingFileResult tests
   **************************************************************************/
  group('isMatchingFileResult tests', () {
    test('test isMatchingFileResult no extensions no patterns', () {
      var config = FindConfig();
      var settings = getSettings();
      var finder = Finder(config, settings);
      var fileResult =
          FileResult(File('./FileUtil.cs'), FileType.code, 0, null);
      expect(finder.isMatchingFileResult(fileResult), true);
    });

    test('test isMatchingFileResult file extension matches inExtensions', () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addExtensions('cs', settings.inExtensions);
      var finder = Finder(config, settings);
      var fileResult =
          FileResult(File('./FileUtil.cs'), FileType.code, 0, null);
      expect(finder.isMatchingFileResult(fileResult), true);
    });

    test('test isMatchingFileResult file extension does not match inExtensions',
        () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addExtensions('java', settings.inExtensions);
      var finder = Finder(config, settings);
      var fileResult =
          FileResult(File('./FileUtil.cs'), FileType.code, 0, null);
      expect(finder.isMatchingFileResult(fileResult), false);
    });

    test('test isMatchingFileResult file extension matches outExtensions', () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addExtensions('cs', settings.outExtensions);
      var finder = Finder(config, settings);
      var fileResult =
          FileResult(File('./FileUtil.cs'), FileType.code, 0, null);
      expect(finder.isMatchingFileResult(fileResult), false);
    });

    test(
        'test isMatchingFileResult file extension does not match outExtensions',
        () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addExtensions('java', settings.outExtensions);
      var finder = Finder(config, settings);
      var fileResult =
          FileResult(File('./FileUtil.cs'), FileType.code, 0, null);
      expect(finder.isMatchingFileResult(fileResult), true);
    });

    test('test isMatchingFileResult file name matches inFilePatterns', () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addPattern('Find', settings.inFilePatterns);
      var finder = Finder(config, settings);
      var fileResult = FileResult(File('./Finder.cs'), FileType.code, 0, null);
      expect(finder.isMatchingFileResult(fileResult), true);
    });

    test('test isMatchingFileResult file name does not match inFilePatterns',
        () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addPattern('Find', settings.inFilePatterns);
      var finder = Finder(config, settings);
      var fileResult =
          FileResult(File('./FileUtil.cs'), FileType.code, 0, null);
      expect(finder.isMatchingFileResult(fileResult), false);
    });

    test('test isMatchingFileResult file name matches outFilePatterns', () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addPattern('Find', settings.outFilePatterns);
      var finder = Finder(config, settings);
      var fileResult = FileResult(File('./Finder.cs'), FileType.code, 0, null);
      expect(finder.isMatchingFileResult(fileResult), false);
    });

    test('test isMatchingFileResult file name does not match outFilePatterns',
        () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addPattern('Find', settings.outFilePatterns);
      var finder = Finder(config, settings);
      var fileResult =
          FileResult(File('./FileUtil.cs'), FileType.code, 0, null);
      expect(finder.isMatchingFileResult(fileResult), true);
    });
  });

  /***************************************************************************
   * isMatchingArchiveFileResult tests
   **************************************************************************/
  group('isMatchingArchiveFileResult tests', () {
    test('test isMatchingArchiveFileResult no extensions no patterns', () {
      var config = FindConfig();
      var settings = getSettings();
      var finder = Finder(config, settings);
      var fileResult =
          FileResult(File('archive.zip'), FileType.archive, 0, null);
      expect(finder.isMatchingArchiveFileResult(fileResult), true);
    });

    test(
        'test isMatchingArchiveFileResult file extension matches inArchiveExtensions',
        () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addExtensions('zip', settings.inArchiveExtensions);
      var finder = Finder(config, settings);
      var fileResult =
          FileResult(File('archive.zip'), FileType.archive, 0, null);
      expect(finder.isMatchingArchiveFileResult(fileResult), true);
    });

    test(
        'test isMatchingArchiveFileResult file extension does not match inArchiveExtensions',
        () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addExtensions('gz', settings.inArchiveExtensions);
      var finder = Finder(config, settings);
      var fileResult =
          FileResult(File('archive.zip'), FileType.archive, 0, null);
      expect(finder.isMatchingArchiveFileResult(fileResult), false);
    });

    test(
        'test isMatchingArchiveFileResult file extension matches outArchiveExtensions',
        () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addExtensions('zip', settings.outArchiveExtensions);
      var finder = Finder(config, settings);
      var fileResult =
          FileResult(File('archive.zip'), FileType.archive, 0, null);
      expect(finder.isMatchingArchiveFileResult(fileResult), false);
    });

    test(
        'test isMatchingArchiveFileResult file extension does not match outArchiveExtensions',
        () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addExtensions('gz', settings.outArchiveExtensions);
      var finder = Finder(config, settings);
      var fileResult =
          FileResult(File('archive.zip'), FileType.archive, 0, null);
      expect(finder.isMatchingArchiveFileResult(fileResult), true);
    });

    test(
        'test isMatchingArchiveFileResult file name matches inArchiveFilePatterns',
        () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addPattern('arch', settings.inArchiveFilePatterns);
      var finder = Finder(config, settings);
      var fileResult =
          FileResult(File('archive.zip'), FileType.archive, 0, null);
      expect(finder.isMatchingArchiveFileResult(fileResult), true);
    });

    test(
        'test isMatchingArchiveFileResult file name does not match inArchiveFilePatterns',
        () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addPattern('archives', settings.inArchiveFilePatterns);
      var finder = Finder(config, settings);
      var fileResult =
          FileResult(File('archive.zip'), FileType.archive, 0, null);
      expect(finder.isMatchingArchiveFileResult(fileResult), false);
    });

    test(
        'test isMatchingArchiveFileResult file name matches outArchiveFilePatterns',
        () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addPattern('arch', settings.outArchiveFilePatterns);
      var finder = Finder(config, settings);
      var fileResult =
          FileResult(File('archive.zip'), FileType.archive, 0, null);
      expect(finder.isMatchingArchiveFileResult(fileResult), false);
    });

    test(
        'test isMatchingArchiveFileResult file name does not match outArchiveFilePatterns',
        () {
      var config = FindConfig();
      var settings = getSettings();
      settings.addPattern('archives', settings.outArchiveFilePatterns);
      var finder = Finder(config, settings);
      var fileResult =
          FileResult(File('archive.zip'), FileType.archive, 0, null);
      expect(finder.isMatchingArchiveFileResult(fileResult), true);
    });
  });

  /***************************************************************************
   * followSymlink tests
   **************************************************************************/
  group('followSymlink tests', () {
    test('test followSymlinks with default settings', () {
      var config = FindConfig();
      var settings = FindSettings();
      settings.paths.add("${config.xFindPath}/bin");
      var finder = Finder(config, settings);
      finder.find().then((fileResults) {
        expect(fileResults.length, lessThan(4));
      });
    });

    test('test followSymlinks with followSymlinks', () {
      var config = FindConfig();
      var settings = FindSettings();
      settings.paths.add("${config.xFindPath}/bin");
      settings.followSymlinks = true;
      var finder = Finder(config, settings);
      finder.find().then((fileResults) {
        if (fileResults.isNotEmpty) {
          expect(fileResults.length, greaterThan(2));
        } else {
          expect(fileResults.length, 0);
        }
      });
    });

    test('test followSymlinks with noFollowSymlinks', () {
      var config = FindConfig();
      var settings = FindSettings();
      settings.paths.add("${config.xFindPath}/bin");
      settings.followSymlinks = false;
      var finder = Finder(config, settings);
      finder.find().then((fileResults) {
        expect(fileResults.length, lessThan(4));
      });
    });
  });
}
