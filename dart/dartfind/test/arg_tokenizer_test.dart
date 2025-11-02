import 'package:dartfind/dartfind.dart' show FindOptions;
import 'package:test/test.dart';

void main() {
  test('test get tokens from minimal args', () async {
    var options = FindOptions();
    await options.ready;
    var tokens = options.argTokenizer!.tokenizeArgs(['.']);
    expect(tokens.length, 1);
    expect(tokens[0].name, 'path');
    expect(tokens[0].value, '.');
  });

  test('test get tokens from valid args', () async {
    var options = FindOptions();
    await options.ready;
    var tokens = options.argTokenizer!.tokenizeArgs(['-x', 'dart,kt', '.']);
    expect(tokens.length, 2);
    expect(tokens[0].name, 'in-ext');
    expect(tokens[0].value, 'dart,kt');
    expect(tokens[1].name, 'path');
    expect(tokens[1].value, '.');
  });

  test('test get settings from json', () async {
    var json = '{'
        '"path": "~/src/xfind/",'
        '"in-ext": ["js","ts"],'
        '"out-dirpattern": ["build", "node_module", "tests", "typings"],'
        r'"out-filepattern": ["gulpfile", "\\.min\\."],'
        '"debug": true,'
        '"followsymlinks": true,'
        '"includehidden": false'
        '}';
    var options = FindOptions();
    await options.ready;
    var tokens = options.argTokenizer!.tokenizeJson(json);
    expect(tokens.length, 12);
    // tokens will be alphabetically ordered by name
    expect(tokens[0].name, 'debug');
    expect(tokens[0].value, true);
    expect(tokens[1].name, 'followsymlinks');
    expect(tokens[1].value, true);
    expect(tokens[2].name, 'in-ext');
    expect(tokens[2].value, 'js');
    expect(tokens[3].name, 'in-ext');
    expect(tokens[3].value, 'ts');
    expect(tokens[4].name, 'includehidden');
    expect(tokens[4].value, false);
    expect(tokens[5].name, 'out-dirpattern');
    expect(tokens[5].value, 'build');
    expect(tokens[9].name, 'out-filepattern');
    expect(tokens[9].value, 'gulpfile');
    expect(tokens[11].name, 'path');
    expect(tokens[11].value, '~/src/xfind/');
  });
}
