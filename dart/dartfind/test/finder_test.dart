import 'dart:io';

import 'package:dartfind/dartfind.dart';
import 'package:test/test.dart';

void main() {
  FindSettings getSettings() {
    var settings = FindSettings();
    settings.paths.add('.');
    return settings;
  }

  /***************************************************************************
   * isFindDir tests
   **************************************************************************/
  group('isFindDir tests', () {
    test('test isFindDir single dot', () {
      var settings = getSettings();
      var finder = Finder(settings);
      expect(finder.isFindDir(Directory('.')), true);
    });

    test('test isFindDir double dot', () {
      var settings = getSettings();
      var finder = Finder(settings);
      expect(finder.isFindDir(Directory('..')), true);
    });

    test('test isFindDir hidden dir', () {
      var settings = getSettings();
      var finder = Finder(settings);
      expect(finder.isFindDir(Directory('.git')), false);
    });

    test('test isFindDir hidden dir includeHidden', () {
      var settings = getSettings();
      settings.excludeHidden = false;
      var finder = Finder(settings);
      expect(finder.isFindDir(Directory('.git')), true);
    });

    test('test isFindDir no patterns', () {
      var settings = getSettings();
      settings.excludeHidden = false;
      var finder = Finder(settings);
      expect(finder.isFindDir(Directory('/Users')), true);
    });

    test('test isFindDir dir matches inDirPatterns', () {
      var settings = getSettings();
      settings.addPattern('Find', settings.inDirPatterns);
      var finder = Finder(settings);
      expect(finder.isFindDir(Directory('./CsFind')), true);
    });

    test('test isFindDir dir does not match inDirPatterns', () {
      var settings = getSettings();
      settings.addPattern('FindFiles', settings.inDirPatterns);
      var finder = Finder(settings);
      expect(finder.isFindDir(Directory('./CsFind')), false);
    });

    test('test isFindDir dir matches outDirPatterns', () {
      var settings = getSettings();
      settings.addPattern('Find', settings.outDirPatterns);
      var finder = Finder(settings);
      expect(finder.isFindDir(Directory('./CsFind')), false);
    });

    test('test isFindDir dir does not match outDirPatterns', () {
      var settings = getSettings();
      settings.addPattern('FindFiles', settings.outDirPatterns);
      var finder = Finder(settings);
      expect(finder.isFindDir(Directory('./CsFind')), true);
    });
  });

  /***************************************************************************
   * isFindFile tests
   **************************************************************************/
  group('isFindFile tests', () {
    test('test isFindFile no extensions no patterns', () {
      var settings = getSettings();
      var finder = Finder(settings);
      var findFile = FindFile(File('./FileUtil.cs'), FileType.code);
      expect(finder.isFindFile(findFile), true);
    });

    test('test isFindFile file extension matches inExtensions', () {
      var settings = getSettings();
      settings.addExtensions('cs', settings.inExtensions);
      var finder = Finder(settings);
      var findFile = FindFile(File('./FileUtil.cs'), FileType.code);
      expect(finder.isFindFile(findFile), true);
    });

    test('test isFindFile file extension does not match inExtensions', () {
      var settings = getSettings();
      settings.addExtensions('java', settings.inExtensions);
      var finder = Finder(settings);
      var findFile = FindFile(File('./FileUtil.cs'), FileType.code);
      expect(finder.isFindFile(findFile), false);
    });

    test('test isFindFile file extension matches outExtensions', () {
      var settings = getSettings();
      settings.addExtensions('cs', settings.outExtensions);
      var finder = Finder(settings);
      var findFile = FindFile(File('./FileUtil.cs'), FileType.code);
      expect(finder.isFindFile(findFile), false);
    });

    test('test isFindFile file extension does not match outExtensions', () {
      var settings = getSettings();
      settings.addExtensions('java', settings.outExtensions);
      var finder = Finder(settings);
      var findFile = FindFile(File('./FileUtil.cs'), FileType.code);
      expect(finder.isFindFile(findFile), true);
    });

    test('test isFindFile file name matches inFilePatterns', () {
      var settings = getSettings();
      settings.addPattern('Find', settings.inFilePatterns);
      var finder = Finder(settings);
      var findFile = FindFile(File('./Finder.cs'), FileType.code);
      expect(finder.isFindFile(findFile), true);
    });

    test('test isFindFile file name does not match inFilePatterns', () {
      var settings = getSettings();
      settings.addPattern('Find', settings.inFilePatterns);
      var finder = Finder(settings);
      var findFile = FindFile(File('./FileUtil.cs'), FileType.code);
      expect(finder.isFindFile(findFile), false);
    });

    test('test isFindFile file name matches outFilePatterns', () {
      var settings = getSettings();
      settings.addPattern('Find', settings.outFilePatterns);
      var finder = Finder(settings);
      var findFile = FindFile(File('./Finder.cs'), FileType.code);
      expect(finder.isFindFile(findFile), false);
    });

    test('test isFindFile file name does not match outFilePatterns', () {
      var settings = getSettings();
      settings.addPattern('Find', settings.outFilePatterns);
      var finder = Finder(settings);
      var findFile = FindFile(File('./FileUtil.cs'), FileType.code);
      expect(finder.isFindFile(findFile), true);
    });
  });

  /***************************************************************************
   * isArchiveFindFile tests
   **************************************************************************/
  group('isArchiveFindFile tests', () {
    test('test isArchiveFindFile no extensions no patterns', () {
      var settings = getSettings();
      var finder = Finder(settings);
      var findFile = FindFile(File('archive.zip'), FileType.archive);
      expect(finder.isArchiveFindFile(findFile), true);
    });

    test('test isArchiveFindFile file extension matches inArchiveExtensions', () {
      var settings = getSettings();
      settings.addExtensions('zip', settings.inArchiveExtensions);
      var finder = Finder(settings);
      var findFile = FindFile(File('archive.zip'), FileType.archive);
      expect(finder.isArchiveFindFile(findFile), true);
    });

    test('test isArchiveFindFile file extension does not match inArchiveExtensions', () {
      var settings = getSettings();
      settings.addExtensions('gz', settings.inArchiveExtensions);
      var finder = Finder(settings);
      var findFile = FindFile(File('archive.zip'), FileType.archive);
      expect(finder.isArchiveFindFile(findFile), false);
    });

    test('test isArchiveFindFile file extension matches outArchiveExtensions', () {
      var settings = getSettings();
      settings.addExtensions('zip', settings.outArchiveExtensions);
      var finder = Finder(settings);
      var findFile = FindFile(File('archive.zip'), FileType.archive);
      expect(finder.isArchiveFindFile(findFile), false);
    });

    test('test isArchiveFindFile file extension does not match outArchiveExtensions', () {
      var settings = getSettings();
      settings.addExtensions('gz', settings.outArchiveExtensions);
      var finder = Finder(settings);
      var findFile = FindFile(File('archive.zip'), FileType.archive);
      expect(finder.isArchiveFindFile(findFile), true);
    });

    test('test isArchiveFindFile file name matches inArchiveFilePatterns', () {
      var settings = getSettings();
      settings.addPattern('arch', settings.inArchiveFilePatterns);
      var finder = Finder(settings);
      var findFile = FindFile(File('archive.zip'), FileType.archive);
      expect(finder.isArchiveFindFile(findFile), true);
    });

    test('test isArchiveFindFile file name does not match inArchiveFilePatterns', () {
      var settings = getSettings();
      settings.addPattern('archives', settings.inArchiveFilePatterns);
      var finder = Finder(settings);
      var findFile = FindFile(File('archive.zip'), FileType.archive);
      expect(finder.isArchiveFindFile(findFile), false);
    });

    test('test isArchiveFindFile file name matches outArchiveFilePatterns', () {
      var settings = getSettings();
      settings.addPattern('arch', settings.outArchiveFilePatterns);
      var finder = Finder(settings);
      var findFile = FindFile(File('archive.zip'), FileType.archive);
      expect(finder.isArchiveFindFile(findFile), false);
    });

    test('test isArchiveFindFile file name does not match outArchiveFilePatterns', () {
      var settings = getSettings();
      settings.addPattern('archives', settings.outArchiveFilePatterns);
      var finder = Finder(settings);
      var findFile = FindFile(File('archive.zip'), FileType.archive);
      expect(finder.isArchiveFindFile(findFile), true);
    });
  });
}
