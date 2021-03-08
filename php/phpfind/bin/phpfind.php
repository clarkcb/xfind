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

        if ($settings->printusage) {
            Logger::log_msg('');
            $findoptions->usage();
            exit;
        }

        $finder = new Finder($settings);
        $findfiles = $finder->find();

        // print matching dirs
        if ($settings->listdirs) {
            $finder->print_matching_dirs($findfiles);
        }

        // print matching files
        if ($settings->listfiles) {
            $finder->print_matching_files($findfiles);
        }
    } catch (FindException $e) {
        Logger::log_msg("\nERROR: " . $e->getMessage() . "\n");
        $findoptions->usage();
    }
}

main($argv);

?>