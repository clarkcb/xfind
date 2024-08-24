<?php

declare(strict_types=1);

namespace phpfind;

$xfind_path = getenv('XFIND_PATH');
if (!$xfind_path) {
    $home = getenv('HOME');
    if ($home) {
        $xfind_path = FileUtil::join_paths($home, 'src', 'xfind');
    } else {
        $xfind_path = FileUtil::join_paths(__DIR__, '..', '..', '..', '..');
    }
}


$shared_path = FileUtil::join_paths($xfind_path, 'shared');

$resources_path = FileUtil::join_paths(__DIR__, '..', '..', 'resources');
$file_types_path = FileUtil::join_paths($resources_path, 'filetypes.json');
$find_options_path = FileUtil::join_paths($resources_path, 'findoptions.json');

define('Z_XFIND_PATH', $xfind_path);
define('Z_SHARED_PATH', $shared_path);
define('Z_FILE_TYPES_PATH', $file_types_path);
define('Z_FIND_OPTIONS_PATH', $find_options_path);

class Config
{
    const string XFIND_PATH = Z_XFIND_PATH;
    const string SHARED_PATH = Z_SHARED_PATH;
    const string FILE_TYPES_PATH = Z_FILE_TYPES_PATH;
    const string FIND_OPTIONS_PATH = Z_FIND_OPTIONS_PATH;
}
