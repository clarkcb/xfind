import 'dart:io' show Platform;

final String xFindPath = Platform.environment.containsKey('XFIND_PATH')
    ? Platform.environment['XFIND_PATH']!
    : '${Platform.environment['HOME']!}/src/xfind';
final String sharedPath = '$xFindPath/shared';
final String fileTypesPath = '$sharedPath/filetypes.json';
final String findOptionsPath = '$sharedPath/findoptions.json';
