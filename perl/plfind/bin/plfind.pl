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

sub handle_err {
    my ($err, $find_options, $colorize) = @_;
    plfind::common::log_msg('');
    plfind::common::log_err($err, $colorize);
    plfind::common::log_msg('');
    $find_options->usage();
    plfind::common::log_msg('');
    exit;
}

sub main {
    my $find_options = plfind::FindOptions->new();
    my ($settings, $errs) = $find_options->settings_from_args(\@ARGV);

    if (scalar @$errs) {
        handle_err($errs->[0], $find_options, 1);
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
        handle_err($errs2->[0], $find_options, $settings->{colorize});
    }

    my ($file_results, $errs3) = $finder->find();
    if (scalar @$errs3) {
        handle_err($errs3->[0], $find_options, $settings->{colorize});
    }

    my $formatter;
    if (scalar @$file_results) {
        $formatter = plfind::FileResultFormatter->new($settings);
    }

    # print matching dirs
    if ($settings->{print_dirs}) {
        plfind::Finder::print_matching_dirs($file_results, $formatter);
    }

    # print matching files
    if ($settings->{print_files}) {
        plfind::Finder::print_matching_files($file_results, $formatter);
    }
}

main();
