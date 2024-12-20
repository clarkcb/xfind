#!/usr/bin/perl -w
#
# findoptions_test.pl
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

use Test::Simple tests => 40;

use plfind::FindOptions;

my $find_options = plfind::FindOptions->new();

sub test_no_args {
    my $args = [];
    my ($settings, $errs) = $find_options->settings_from_args($args);
    ok(scalar @{$errs} == 0, 'No errors from empty args');
    ok(!$settings->{archives_only}, 'archives_only is false by default');
    ok(!$settings->{debug}, 'debug is false by default');
    ok(!$settings->{follow_symlinks}, 'follow_symlinks is false by default');
    ok(!$settings->{include_archives}, 'include_archives is false by default');
    ok(!$settings->{include_hidden}, 'include_hidden is false by default');
    ok(!$settings->{print_dirs}, 'print_dirs is false by default');
    ok($settings->{print_files}, 'print_files is true by default');
    ok(!$settings->{print_usage}, 'print_usage is false by default');
    ok(!$settings->{print_version}, 'print_version is false by default');
    ok($settings->{recursive}, 'recursive is true by default');
    ok(scalar @{$settings->{paths}} == 0, 'paths are empty by default');
    ok(!$settings->{verbose}, 'verbose is false by default');
}

sub test_valid_args {
    my $args = ['-x', 'pl,py', '.'];
    my ($settings, $errs) = $find_options->settings_from_args($args);
    ok(scalar @{$errs} == 0, 'No errors from valid args');
    ok(scalar @{$settings->{in_extensions}} == 2, 'in_extensions has two extensions');
    ok($settings->{in_extensions}->[0] eq 'pl', 'in_extensions has "pl" extension');
    ok($settings->{in_extensions}->[1] eq 'py', 'in_extensions has "py" extension');
    ok(${$settings->{paths}}[0] eq '.', 'paths[0] eq '.'');
}

sub test_archives_only_arg {
    my $args = ['--archivesonly'];
    my ($settings, $errs) = $find_options->settings_from_args($args);
    ok(scalar @{$errs} == 0, 'No errors from valid archives_only arg');
    ok($settings->{archives_only}, 'archives_only is true');
    ok($settings->{include_archives}, 'include_archives is true');
}

sub test_debug_arg {
    my $args = ['--debug'];
    my ($settings, $errs) = $find_options->settings_from_args($args);
    ok(scalar @{$errs} == 0, 'No errors from valid debug arg');
    ok($settings->{debug}, 'debug is true');
    ok($settings->{verbose}, 'verbose is true');
}

sub test_missing_arg {
    my $args = ['-x'];
    my ($settings, $errs) = $find_options->settings_from_args($args);
    ok(scalar @{$errs} == 1, 'Error from missing value for arg');
    ok($errs->[0] eq 'Missing value for x', 'Correct missing value error message');
}

sub test_invalid_arg {
    my $args = ['-Q'];
    my ($settings, $errs) = $find_options->settings_from_args($args);
    ok(scalar @{$errs} == 1, 'Error from unknown arg');
    ok($errs->[0] eq 'Invalid option: Q', 'Correct unknown option error message');
}

sub test_settings_from_json {
    my $settings = plfind::FindSettings->new();
    my $json = <<"END_JSON";
{
  "path": "~/src/xfind/",
  "in-ext": ["js","ts"],
  "out-dirpattern": "node_module",
  "out-filepattern": ["temp"],
  "debug": true,
  "followsymlinks": true,
  "includehidden": true
}
END_JSON
    $find_options->settings_from_json($json, $settings);
    # path is expanded when added to settings, must compare to expanded version
    # my $expected_path = $ENV{HOME} . "/src/xfind";
    my $expected_path = "~/src/xfind";
    ok(${$settings->{paths}}[0]->stringify eq $expected_path, "paths[0] is set to ~/src/xfind");
    ok(scalar @{$settings->{in_extensions}} == 2, "in_extensions has two extensions");
    ok($settings->{in_extensions}->[0] eq 'js', "in_extensions contains js extension");
    ok($settings->{in_extensions}->[1] eq 'ts', "in_extensions contains ts extension");
    ok(scalar @{$settings->{out_dir_patterns}} == 1, "out_dir_patterns has one pattern");
    ok($settings->{out_dir_patterns}->[0] eq 'node_module', "out_dir_patterns[0] is node_module");
    ok(scalar @{$settings->{out_file_patterns}} == 1, "out_file_patterns has one pattern");
    ok($settings->{out_file_patterns}->[0] eq 'temp', "out_file_patterns[0] is temp");
    ok($settings->{debug} == 1, "debug is set to true");
    ok($settings->{follow_symlinks} == 1, "follow_symlinks is set to true");
    ok($settings->{verbose} == 1, "verbose is set to true");
    ok($settings->{include_hidden} == 1, 'include_hidden is set to true');
}

sub main {
    test_no_args();
    test_valid_args();
    test_archives_only_arg();
    test_debug_arg();
    test_missing_arg();
    test_invalid_arg();
    test_settings_from_json();
}

main();
