import 'package:dartfind/dartfind.dart' show FindOptions, FindSettings;
import 'package:test/test.dart';

void main() {
  test('test get settings from minimal args', () async {
    var options = FindOptions();
    var settings = await options.settingsFromArgs(['.']);
    expect(settings.archivesOnly, false);
    expect(settings.colorize, true);
    expect(settings.debug, false);
    expect(settings.excludeHidden, true);
    expect(settings.includeArchives, false);
    expect(settings.listDirs, false);
    expect(settings.listFiles, true);
    expect(settings.printUsage, false);
    expect(settings.printVersion, false);
    expect(settings.recursive, true);
    expect(settings.verbose, false);
    expect(settings.paths.length, 1);
  });

  test('test get settings from valid args', () async {
    var options = FindOptions();
    var settings = await options.settingsFromArgs(['-x', 'dart,kt', '.']);
    expect(settings.inExtensions.length, 2);
    expect(settings.inExtensions.contains('dart'), true);
    expect(settings.inExtensions.contains('kt'), true);
    expect(settings.paths.length, 1);
    expect(settings.paths.first, '.');
  });

  test('test get settings from json', () async {
    var json = '{'
    '"path": "~/src/xfind/",'
    '"in-ext": ["js","ts"],'
    '"out-dirpattern": ["build", "node_module", "tests", "typings"],'
    r'"out-filepattern": ["gulpfile", "\\.min\\."],'
    '"debug": true,'
    '"includehidden": false'
    '}';
    var options = FindOptions();
    var settings = FindSettings();
    await options.settingsFromJson(json, settings);

    expect(settings.paths.length, 1);
    expect(settings.paths.first, '~/src/xfind/');

    expect(settings.inExtensions.length, 2);
    expect(settings.inExtensions.contains('js'), true);
    expect(settings.inExtensions.contains('ts'), true);

    expect(settings.outDirPatterns.length, 4);
    expect(settings.outDirPatterns.any((p) => (p as RegExp).pattern == 'node_module'), true);

    expect(settings.outFilePatterns.length, 2);
    expect(settings.outFilePatterns.any((p) => (p as RegExp).pattern == 'gulpfile'), true);

    expect(settings.debug, true);
    expect(settings.verbose, true);
    expect(settings.excludeHidden, true);
  });
}
