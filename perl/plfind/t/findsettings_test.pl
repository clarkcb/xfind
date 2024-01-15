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

use Test::Simple tests => 23;

use plfind::FindSettings;

sub test_default_settings {
    my $settings = new plfind::FindSettings();
    ok(!$settings->{archives_only}, "archives_only is false by default");
    ok(!$settings->{debug}, "debug is false by default");
    ok(!$settings->{include_archives}, "include_archives is false by default");
    ok(!$settings->{include_hidden}, "include_hidden is false by default");
    ok(@{$settings->{paths}} eq 0, "paths are empty by default");
    ok(!$settings->{print_dirs}, "print_dirs is false by default");
    ok(!$settings->{print_files}, "print_files is false by default");
    ok(!$settings->{print_usage}, "print_usage is false by default");
    ok(!$settings->{print_version}, "print_version is false by default");
    ok($settings->{recursive}, "recursive is true by default");
    ok(!$settings->{sort_case_insensitive}, "sort_case_insensitive is false by default");
    ok(!$settings->{sort_descending}, "sort_descending is false by default");
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
    $settings->add_patterns('Find', $settings->{in_file_patterns});
    ok(scalar @{$settings->{in_file_patterns}} == 1, "in_file_patterns has one pattern");
}

sub test_add_array_patterns {
    my $settings = new plfind::FindSettings();
    $settings->add_patterns(['Finder', 'Result'], $settings->{in_file_patterns});
    ok(scalar @{$settings->{in_file_patterns}} == 2, "in_file_patterns has two patterns");
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
