#!/usr/bin/env php
<?php

declare(strict_types=1);

require_once __DIR__ . '/../src/autoload.php';

use phpfind\FileResultFormatter;
use phpfind\Logger;
use phpfind\FindOptions;
use phpfind\Finder;
use phpfind\FindException;

function main($argv): void
{
    $find_options = new FindOptions();
    try {
        $settings = $find_options->settings_from_args(array_slice($argv, 1));
        if ($settings->debug) {
            Logger::log_msg("settings: $settings");
        }

        if ($settings->print_usage) {
            Logger::log_msg('');
            $find_options->usage_and_exit(0);
        }

        $finder = new Finder($settings);
        $file_results = $finder->find();
        $formatter = new FileResultFormatter($settings);

        // print matching dirs
        if ($settings->print_dirs) {
            $finder->print_matching_dirs($file_results, $formatter);
        }

        // print matching files
        if ($settings->print_files) {
            $finder->print_matching_files($file_results, $formatter);
        }
    } catch (FindException $e) {
        Logger::log_msg('');
        Logger::log_err($e->getMessage() . "\n");
        $find_options->usage_and_exit(1);
    }
}

main($argv);

?>