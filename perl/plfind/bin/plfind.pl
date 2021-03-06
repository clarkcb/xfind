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
    my $findoptions = new plfind::FindOptions();
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

    if ($settings->{printusage}) {
        plfind::common::log('');
        $findoptions->usage();
        plfind::common::log('');
        exit;
    }

    my ($finder, $errs2) = new plfind::Finder($settings);

    if (scalar @{$errs2}) {
        plfind::common::log('');
        log_error($errs2->[0]);
        plfind::common::log('');
        $findoptions->usage();
        plfind::common::log('');
        exit;
    }

    my $findfiles = $finder->find();

    # print matching dirs
    if ($settings->{listdirs}) {
        $finder->print_matching_dirs($findfiles);
    }

    # print matching files
    if ($settings->{listfiles}) {
        $finder->print_matching_files($findfiles);
    }
}

main();
