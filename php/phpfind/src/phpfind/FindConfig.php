<?php

declare(strict_types=1);

namespace phpfind;

/**
 * Class FindConfig
 */
readonly class FindConfig
{
    public string $file_types_path;
    public string $find_options_path;
    public string $default_find_settings_path;

    public function __construct()
    {
        $home = getenv('HOME');

        $resources_path = FileUtil::join_paths(__DIR__, '..', '..', 'resources');
        $file_types_path = FileUtil::join_paths($resources_path, 'filetypes.json');
        $find_options_path = FileUtil::join_paths($resources_path, 'findoptions.json');
        $default_find_settings_path = FileUtil::join_paths($home, '.config', 'xfind', 'settings.json');

        $this->file_types_path = $file_types_path;
        $this->find_options_path = $find_options_path;
        $this->default_find_settings_path = $default_find_settings_path;
    }
}
