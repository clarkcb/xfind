import 'dart:io' show Platform;

String xFindPath = Platform.environment.containsKey('XFIND_PATH')
    ? Platform.environment['XFIND_PATH']!
    : '${Platform.environment['HOME']!}/src/xfind';
String sharedPath = '$xFindPath/shared';
String fileTypesPath = '$sharedPath/filetypes.json';
String findOptionsPath = '$sharedPath/findoptions.json';
