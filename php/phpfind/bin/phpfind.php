#!/usr/bin/env php
<?php

require_once __DIR__ . '/../src/autoload.php';

use \phpfind\Logger;
use \phpfind\FindOptions;
use \phpfind\Finder;
use \phpfind\FindException;

function main($argv)
{
    $findoptions = new FindOptions();
    try {
        $settings = $findoptions->settings_from_args(array_slice($argv, 1));
        if ($settings->debug) {
            Logger::log_msg("settings: $settings");
        }

        if ($settings->print_usage) {
            Logger::log_msg('');
            $findoptions->usage_and_exit(0);
        }

        $finder = new Finder($settings);
        $file_results = $finder->find();

        // print matching dirs
        if ($settings->list_dirs) {
            $finder->print_matching_dirs($file_results);
        }

        // print matching files
        if ($settings->list_files) {
            $finder->print_matching_files($file_results);
        }
    } catch (FindException $e) {
        Logger::log_msg("\nERROR: " . $e->getMessage() . "\n");
        $findoptions->usage_and_exit(1);
    }
}

main($argv);

?>