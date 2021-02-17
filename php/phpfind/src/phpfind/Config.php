<?php declare(strict_types=1);

namespace phpfind;

$config_json_path = __DIR__ . '/../../config/config.json';
$config = json_decode(file_get_contents($config_json_path));

$sharedpath = $config->{'xfindpath'} . '/shared';

$resources_path = __DIR__ . '/../../resources';

define('Z_XFINDPATH', $config->{'xfindpath'});
define('Z_SHAREDPATH', $sharedpath);
define('Z_FILETYPESPATH', $resources_path . '/filetypes.json');
define('Z_FINDOPTIONSPATH', $resources_path . '/findoptions.json');

class Config
{
    const XFINDPATH = Z_XFINDPATH;
    const SHAREDPATH = Z_SHAREDPATH;
    const FILETYPESPATH = Z_FILETYPESPATH;
    const FINDOPTIONSPATH = Z_FINDOPTIONSPATH;
}
