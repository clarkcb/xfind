import 'package:dartfind/dartfind.dart' show FileUtil;
import 'package:test/test.dart';

void main() {
  /***************************************************************************
   * extension tests
   **************************************************************************/
  group('extension tests', () {
    test('test extension file with extension', () {
      expect(FileUtil.extension('filename.txt'), 'txt');
    });

    test('test extension file no extension', () {
      expect(FileUtil.extension('README'), '');
    });

    test('test extension file empty extension', () {
      expect(FileUtil.extension('filewithdot.'), '');
    });

    test('test extension hidden file with extension', () {
      expect(FileUtil.extension('.hidden.txt'), 'txt');
    });

    test('test extension hidden file no extension', () {
      expect(FileUtil.extension('.gitignore'), '');
    });
  });

  /***************************************************************************
   * expandPath tests
   **************************************************************************/
  group('expandPath tests', () {
    var homePath = FileUtil.homePath();
    test('test expandPath tilde', () {
      var tilde = '~';
      expect(FileUtil.expandPath(tilde), homePath);
    });
    test('test expandPath path with tilde', () {
      var tildePath = '~/src/xfind';
      expect(FileUtil.expandPath(tildePath), '$homePath/src/xfind');
    });
    test('test expandPath path with tilde and name', () {
      var tildeNamePath = '~cary/src/xfind';
      expect(FileUtil.expandPath(tildeNamePath), '$homePath/src/xfind');
    });
    test('test expandPath relative path no tilde', () {
      expect(FileUtil.expandPath('./path'), './path');
    });

    test('test expandPath absolute home path no tilde', () {
      expect(FileUtil.expandPath('$homePath/path'), '$homePath/path');
    });
  });

  /***************************************************************************
   * isDotDir tests
   **************************************************************************/
  group('isDotDir tests', () {
    test('test isDotDir single dot', () {
      expect(FileUtil.isDotDir('.'), true);
    });

    test('test isDotDir double dot', () {
      expect(FileUtil.isDotDir('..'), true);
    });

    test('test isDotDir path without dot', () {
      expect(FileUtil.isDotDir('~/path'), false);
    });

    test('test isDotDir path with dot', () {
      expect(FileUtil.isDotDir('./path'), false);
    });

    test('test isDotDir hidden file', () {
      expect(FileUtil.isDotDir('.gitignore'), false);
    });
  });

  /***************************************************************************
   * isHidden tests
   **************************************************************************/
  group('isHidden tests', () {
    test('test isHidden single dot', () {
      expect(FileUtil.isHidden('.'), false);
    });

    test('test isHidden double dot', () {
      expect(FileUtil.isHidden('..'), false);
    });

    test('test isHidden hidden file', () {
      expect(FileUtil.isHidden('.gitignore'), true);
    });

    test('test isHidden not-hidden file', () {
      expect(FileUtil.isHidden('filename.txt'), false);
    });
  });
}
