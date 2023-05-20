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
use plfind::Finder;
use plfind::FindOptions;

sub log_error {
    my $err = shift;
    plfind::common::log('ERROR: '.$err);
}

sub main {
    my $findoptions = plfind::FindOptions->new();
    my ($settings, $errs) = $findoptions->settings_from_args(\@ARGV);

    if (scalar @{$errs}) {
        plfind::common::log('');
        log_error($errs->[0]);
        plfind::common::log('');
        $findoptions->usage();
        plfind::common::log('');
        exit;
    }

    if ($settings->{debug}) {
        print 'settings: ' . $settings->to_string() . "\n";
    }

    if ($settings->{print_usage}) {
        plfind::common::log('');
        $findoptions->usage();
        plfind::common::log('');
        exit;
    }

    my ($finder, $errs2) = plfind::Finder->new($settings);

    if (scalar @{$errs2}) {
        plfind::common::log('');
        log_error($errs2->[0]);
        plfind::common::log('');
        $findoptions->usage();
        plfind::common::log('');
        exit;
    }

    my $fileresults = $finder->find();

    # print matching dirs
    if ($settings->{list_dirs}) {
        $finder->print_matching_dirs($fileresults);
    }

    # print matching files
    if ($settings->{list_files}) {
        $finder->print_matching_files($fileresults);
    }
}

main();
