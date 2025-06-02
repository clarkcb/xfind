#!/usr/bin/perl -w
#
# plsearch.pl
#
#
use strict;
use warnings;

use Cwd 'abs_path';
use File::Basename;

my $lib_path;

BEGIN {
    $lib_path = dirname(dirname(abs_path($0))) . '/lib';
    unshift @INC, $lib_path;
}

use plfind::common;
use plfind::config;
use plfind::FileResultFormatter;
use plfind::Finder;
use plfind::FindOptions;

sub get_matching_dirs {
    my ($file_results) = @_;
    my @dirs = map {$_->{file_path}->parent} @$file_results;
    my $uniq = plfind::common::uniq(\@dirs);
    return $uniq;
}

sub print_matching_dirs {
    my ($file_results, $formatter) = @_;
    my $dirs = get_matching_dirs($file_results);
    if (scalar @$dirs) {
        plfind::common::log_msg(sprintf("\nMatching directories (%d):", scalar @$dirs));
        foreach my $d (@$dirs) {
            plfind::common::log_msg($formatter->format_dir($d));
        }
    } else {
        plfind::common::log_msg("\nMatching directories: 0");
    }
}

sub print_matching_files {
    my ($file_results, $formatter) = @_;
    if (scalar @$file_results) {
        plfind::common::log_msg(sprintf("\nMatching files (%d):", scalar @$file_results));
        foreach my $fr (@$file_results) {
            plfind::common::log_msg($formatter->format_file_result($fr));
        }
    } else {
        plfind::common::log_msg("\nMatching files: 0");
    }
}

sub main {
    my $find_options = plfind::FindOptions->new();
    my ($settings, $errs) = $find_options->settings_from_args(\@ARGV);

    if (scalar @$errs) {
        plfind::common::log_msg('');
        plfind::common::log_err($errs->[0]);
        plfind::common::log_msg('');
        $find_options->usage();
        plfind::common::log_msg('');
        exit;
    }

    if ($settings->{debug}) {
        print 'settings: ' . $settings->to_string() . "\n";
    }

    if ($settings->{print_usage}) {
        plfind::common::log_msg('');
        $find_options->usage();
        plfind::common::log_msg('');
        exit;
    }

    my ($finder, $errs2) = plfind::Finder->new($settings);

    if (scalar @$errs2) {
        plfind::common::log_msg('');
        plfind::common::log_err($errs2->[0]);
        plfind::common::log_msg('');
        $find_options->usage();
        plfind::common::log_msg('');
        exit;
    }

    my $file_results = $finder->find();
    my $formatter = plfind::FileResultFormatter->new($settings);

    # print matching dirs
    if ($settings->{print_dirs}) {
        print_matching_dirs($file_results, $formatter);
    }

    # print matching files
    if ($settings->{print_files}) {
        print_matching_files($file_results, $formatter);
    }
}

main();
