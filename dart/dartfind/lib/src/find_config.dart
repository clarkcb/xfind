import 'dart:io' show Platform;

class FindConfig {
  String xFindPath = "";
  String fileTypesPath = "";
  String findOptionsPath = "";
  String defaultFindSettingsPath = "";

  FindConfig() {
    xFindPath = Platform.environment.containsKey('XFIND_PATH')
        ? Platform.environment['XFIND_PATH']!
        : '${Platform.environment['HOME']!}/src/xfind';

    fileTypesPath = '$xFindPath/shared/filetypes.json';
    findOptionsPath = '$xFindPath/shared/findoptions.json';
    defaultFindSettingsPath =
        '${Platform.environment['HOME']!}/.config/xfind/settings.json';
  }
}
