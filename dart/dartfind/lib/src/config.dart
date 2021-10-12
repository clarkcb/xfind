import 'dart:io' show Platform;

String XFINDPATH = Platform.environment.containsKey('XFIND_PATH')
    ? Platform.environment['XFIND_PATH']
    : Platform.environment['HOME'] + '/src/xfind';
String SHAREDPATH = '$XFINDPATH/shared';
String FILETYPESPATH = '$SHAREDPATH/filetypes.json';
String FINDOPTIONSPATH = '$SHAREDPATH/findoptions.json';
