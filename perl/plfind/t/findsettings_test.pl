#!/usr/bin/perl -w
#
# findsettings_test.pl
#
#
use strict;
use warnings;

use Cwd 'abs_path';
use File::Basename;

my $lib_path;

BEGIN {
    $lib_path = dirname(dirname(abs_path($0))) . '/lib';
    # print "lib_path: $lib_path\n";
    unshift @INC, $lib_path;
}

use Test::Simple tests => 30;

use plfind::FindSettings;

sub test_default_settings {
    my $settings = new plfind::FindSettings();
    ok(!$settings->{archivesonly}, "archivesonly is false by default");
    ok($settings->{colorize}, "colorize is true by default");
    ok(!$settings->{debug}, "debug is false by default");
    ok($settings->{excludehidden}, "excludehidden is true by default");
    ok(!$settings->{firstmatch}, "firstmatch is false by default");
    ok($settings->{linesafter} == 0, "linesafter == 0 by default");
    ok($settings->{linesbefore} == 0, "linesbefore == 0 by default");
    ok(!$settings->{listdirs}, "listdirs is false by default");
    ok(!$settings->{listfiles}, "listfiles is false by default");
    ok(!$settings->{listlines}, "listlines is false by default");
    ok($settings->{maxlinelength} == 150, "maxlinelength == 150 by default");
    ok(!$settings->{multilineoption-REMOVE}, "multilineoption-REMOVE is false by default");
    ok($settings->{printresults}, "printresults is true by default");
    ok(!$settings->{printusage}, "printusage is false by default");
    ok(!$settings->{printversion}, "printversion is false by default");
    ok($settings->{recursive}, "recursive is true by default");
    ok(!$settings->{findarchives}, "findarchives is false by default");
    ok($settings->{startpath} eq '', "startpath is empty by default");
    ok(!$settings->{uniquelines}, "uniquelines is false by default");
    ok(!$settings->{verbose}, "verbose is false by default");
}

sub test_add_single_extension {
    my $settings = new plfind::FindSettings();
    $settings->add_exts('pl', $settings->{in_extensions});
    ok(scalar @{$settings->{in_extensions}} == 1, "in_extensions has one extension");
    ok($settings->{in_extensions}->[0] eq 'pl', "in_extensions contains pl extension");
}

sub test_add_comma_delimited_extensions {
    my $settings = new plfind::FindSettings();
    $settings->add_exts('pl,py', $settings->{in_extensions});
    ok(scalar @{$settings->{in_extensions}} == 2, "in_extensions has two extensions");
    ok($settings->{in_extensions}->[0] eq 'pl', "in_extensions contains pl extension");
    ok($settings->{in_extensions}->[1] eq 'py', "in_extensions contains py extension");
}

sub test_add_array_extensions {
    my $settings = new plfind::FindSettings();
    $settings->add_exts(['pl','py'], $settings->{in_extensions});
    ok(scalar @{$settings->{in_extensions}} == 2, "in_extensions has two extensions");
    ok($settings->{in_extensions}->[0] eq 'pl', "in_extensions contains pl extension");
    ok($settings->{in_extensions}->[1] eq 'py', "in_extensions contains py extension");
}

sub test_add_single_pattern {
    my $settings = new plfind::FindSettings();
    $settings->add_patterns('Finder', $settings->{findpatterns});
    ok(scalar @{$settings->{findpatterns}} == 1, "findpatterns has one pattern");
}

sub test_add_array_patterns {
    my $settings = new plfind::FindSettings();
    $settings->add_patterns(['Finder', 'Result'], $settings->{findpatterns});
    ok(scalar @{$settings->{findpatterns}} == 2, "findpatterns has two patterns");
}

sub main {
    test_default_settings();
    test_add_single_extension();
    test_add_comma_delimited_extensions();
    test_add_array_extensions();
    test_add_single_pattern();
    test_add_array_patterns();
}

main();
