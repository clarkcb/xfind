#!/usr/bin/perl -w
#
# finder_test.pl
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

use Test::Simple tests => 73;

use plfind::config;
use plfind::FileUtil;
use plfind::FindSettings;
use plfind::Finder;


sub get_settings {
    my $settings = new plfind::FindSettings();
    $settings->{paths} = ['.'];
    return $settings;
}

sub get_test_file {
  return "$SHAREDPATH/testFiles/testFile2.txt";
}

sub test_validate_settings {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
}

################################################################################
# is_matching_dir tests
################################################################################
sub test_is_matching_dir_no_patterns {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'plfind';
    ok($finder->is_matching_dir($dir), "$dir is find dir with no patterns");
}

sub test_is_matching_dir_matches_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_dir_patterns}}, 'plfind');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'plfind';
    ok($finder->is_matching_dir($dir), "$dir matches in_dir_patterns");
}

sub test_is_matching_dir_no_match_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_dir_patterns}}, 'plfind');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'pyfind';
    ok(!$finder->is_matching_dir($dir), "$dir does not match in_dir_patterns");
}

sub test_is_matching_dir_matches_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_dir_patterns}}, 'pyfind');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'pyfind';
    ok(!$finder->is_matching_dir($dir), "$dir matches out_dir_patterns");
}

sub test_is_matching_dir_no_match_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_dir_patterns}}, 'pyfind');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'plfind';
    ok($finder->is_matching_dir($dir), "$dir does not match out_dir_patterns");
}

sub test_is_matching_dir_single_dot {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = '.';
    ok($finder->is_matching_dir($dir), "$dir is find dir");
}

sub test_is_matching_dir_double_dot {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = '..';
    ok($finder->is_matching_dir($dir), "$dir is find dir");
}

sub test_is_matching_dir_hidden_dir {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = '.git';
    ok(!$finder->is_matching_dir($dir), "Hidden dir $dir is not find dir by default");
}

sub test_is_matching_dir_hidden_dir_include_hidden {
    my $settings = get_settings();
    $settings->{exclude_hidden} = 0;
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = '.git';
    ok($finder->is_matching_dir($dir),
        "Hidden dir $dir is find dir with exclude_hidden set to false");
}

################################################################################
# is_matching_file tests
################################################################################
sub test_is_matching_file_matches_by_default {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $d = '.';
    my $f = 'FileUtil.pm';
    my $file_type = plfind::FileType->CODE;
    my $stat = [];
    my $file_result = plfind::FileResult->new($d, $f, $file_type, $stat);
    ok($finder->is_matching_file_result($file_result), "$f is matching file by default");
}

sub test_is_matching_file_matches_in_extension {
    my $settings = get_settings();
    push(@{$settings->{in_extensions}}, 'pm');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $d = '.';
    my $f = 'FileUtil.pm';
    my $file_type = plfind::FileType->CODE;
    my $stat = [];
    my $file_result = plfind::FileResult->new($d, $f, $file_type, $stat);
    ok($finder->is_matching_file_result($file_result), "$f matches in_extensions");
}

sub test_is_matching_file_no_match_in_extension {
    my $settings = get_settings();
    push(@{$settings->{in_extensions}}, 'pl');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $d = '.';
    my $f = 'FileUtil.pm';
    my $file_type = plfind::FileType->CODE;
    my $stat = [];
    my $file_result = plfind::FileResult->new($d, $f, $file_type, $stat);
    ok(!$finder->is_matching_file_result($file_result), "$f does not match in_extensions");
}

sub test_is_matching_file_matches_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_extensions}}, 'pm');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $d = '.';
    my $f = 'FileUtil.pm';
    my $file_type = plfind::FileType->CODE;
    my $stat = [];
    my $file_result = plfind::FileResult->new($d, $f, $file_type, $stat);
    ok(!$finder->is_matching_file_result($file_result), "$f matches out_extensions");
}

sub test_is_matching_file_no_match_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_extensions}}, 'py');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $d = '.';
    my $f = 'FileUtil.pm';
    my $file_type = plfind::FileType->CODE;
    my $stat = [];
    my $file_result = plfind::FileResult->new($d, $f, $file_type, $stat);
    ok($finder->is_matching_file_result($file_result), "$f does not match out_extensions");
}

sub test_is_matching_file_matches_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_file_patterns}}, 'Find');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $d = '.';
    my $f = 'Finder.pm';
    my $file_type = plfind::FileType->CODE;
    my $stat = [];
    my $file_result = plfind::FileResult->new($d, $f, $file_type, $stat);
    ok($finder->is_matching_file_result($file_result), "$f matches in_file_patterns");
}

sub test_is_matching_file_no_match_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_file_patterns}}, 'Find');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $d = '.';
    my $f = 'FileUtil.pm';
    my $file_type = plfind::FileType->CODE;
    my $stat = [];
    my $file_result = plfind::FileResult->new($d, $f, $file_type, $stat);
    ok(!$finder->is_matching_file_result($file_result), "$f does not match in_file_patterns");
}

sub test_is_matching_file_matches_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_file_patterns}}, 'Find');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $d = '.';
    my $f = 'Finder.pm';
    my $file_type = plfind::FileType->CODE;
    my $stat = [];
    my $file_result = plfind::FileResult->new($d, $f, $file_type, $stat);
    ok(!$finder->is_matching_file_result($file_result), "$f matches out_file_patterns");
}

sub test_is_matching_file_no_match_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_file_patterns}}, 'Find');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $d = '.';
    my $f = 'FileUtil.pm';
    my $file_type = plfind::FileType->CODE;
    my $stat = [];
    my $file_result = plfind::FileResult->new($d, $f, $file_type, $stat);
    ok($finder->is_matching_file_result($file_result), "$f does not match out_file_patterns");
}

################################################################################
# is_matching_archive_file tests
################################################################################
sub test_is_matching_archive_file_matches_by_default {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($finder->is_matching_archive_file($file), "$file is archive find file by default");
}

sub test_is_matching_archive_file_matches_in_extension {
    my $settings = get_settings();
    push(@{$settings->{in_archive_extensions}}, 'zip');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($finder->is_matching_archive_file($file), "$file matches in_archive_extensions");
}

sub test_is_matching_archive_file_no_match_in_extension {
    my $settings = get_settings();
    push(@{$settings->{in_archive_extensions}}, 'gz');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(!$finder->is_matching_archive_file($file), "$file does not match in_archive_extensions");
}

sub test_is_matching_archive_file_matches_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_archive_extensions}}, 'zip');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(!$finder->is_matching_archive_file($file), "$file matches out_archive_extensions");
}

sub test_is_matching_archive_file_no_match_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_archive_extensions}}, 'gz');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($finder->is_matching_archive_file($file), "$file does not match out_archive_extensions");
}

sub test_is_matching_archive_file_matches_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_archive_file_patterns}}, 'arch');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($finder->is_matching_archive_file($file), "$file matches in_archive_file_patterns");
}

sub test_is_matching_archive_file_no_match_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_archive_file_patterns}}, 'archives');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(!$finder->is_matching_archive_file($file), "$file does not match in_archive_file_patterns");
}

sub test_is_matching_archive_file_matches_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_archive_patterns}}, 'arch');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(!$finder->is_matching_archive_file($file), "$file matches out_archive_patterns");
}

sub test_is_matching_archive_file_no_match_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_archive_patterns}}, 'archives');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($finder->is_matching_archive_file($file), "$file does not match out_archive_patterns");
}

################################################################################
# filter_file tests
################################################################################
sub test_filter_file_matches_by_default {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok(defined $finder->filter_to_file_result($file), "filter_to_file_result($file) defined by default");
}

sub test_filter_file_is_matching_file {
    my $settings = get_settings();
    push(@{$settings->{in_extensions}}, 'pm');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok(defined $finder->filter_to_file_result($file), "filter_to_file_result($file) defined when is_matching_file");
}

sub test_filter_file_not_is_matching_file {
    my $settings = get_settings();
    push(@{$settings->{in_extensions}}, 'pl');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok(!defined $finder->filter_to_file_result($file), "filter_to_file_result($file) not defined when !is_matching_file");
}

sub test_filter_file_is_hidden_file {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = '.gitignore';
    ok(!defined $finder->filter_to_file_result($file), "filter_to_file_result($file) not defined when exclude_hidden=1");
}

sub test_filter_file_hidden_includehidden {
    my $settings = get_settings();
    $settings->{exclude_hidden} = 0;
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = '.gitignore';
    ok(defined $finder->filter_to_file_result($file), "filter_to_file_result($file) defined when hidden and exclude_hidden=0");
}

sub test_filter_file_archive_no_include_archives {
    my $settings = get_settings();
    $settings->{include_archives} = 0;
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(!defined $finder->filter_to_file_result($file), "filter_to_file_result($file) not defined when include_archives=0");
}

sub test_filter_file_archive_include_archives {
    my $settings = get_settings();
    $settings->{include_archives} = 1;
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(defined $finder->filter_to_file_result($file), "filter_to_file_result($file) defined when include_archives=1");
}

sub test_filter_file_archive_archives_only {
    my $settings = get_settings();
    $settings->{archives_only} = 1;
    $settings->{include_archives} = 1;
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(defined $finder->filter_to_file_result($file), "filter_to_file_result($file) defined when archives_only=1");
}

sub test_filter_file_nonarchive_archives_only {
    my $settings = get_settings();
    $settings->{archives_only} = 1;
    $settings->{include_archives} = 1;
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok(!defined $finder->filter_to_file_result($file), "filter_to_file_result($file) not defined when archives_only=1");
}

################################################################################
# main
################################################################################
sub main {

    test_validate_settings();                               # 1 test

    # is_matching_dir tests
    test_is_matching_dir_no_patterns();                     # 2 tests
    test_is_matching_dir_matches_in_pattern();              # 2 tests
    test_is_matching_dir_no_match_in_pattern();             # 2 tests
    test_is_matching_dir_matches_out_pattern();             # 2 tests
    test_is_matching_dir_no_match_out_pattern();            # 2 tests
    test_is_matching_dir_single_dot();                      # 2 tests
    test_is_matching_dir_double_dot();                      # 2 tests
    test_is_matching_dir_hidden_dir();                      # 2 tests
    test_is_matching_dir_hidden_dir_include_hidden();       # 2 tests

    # is_matching_file tests
    test_is_matching_file_matches_by_default();             # 2 tests
    test_is_matching_file_matches_in_extension();           # 2 tests
    test_is_matching_file_no_match_in_extension();          # 2 tests
    test_is_matching_file_matches_out_extension();          # 2 tests
    test_is_matching_file_no_match_out_extension();         # 2 tests
    test_is_matching_file_matches_in_pattern();             # 2 tests
    test_is_matching_file_no_match_in_pattern();            # 2 tests
    test_is_matching_file_matches_out_pattern();            # 2 tests
    test_is_matching_file_no_match_out_pattern();           # 2 tests

    # is_matching_archive_file tests
    test_is_matching_archive_file_matches_by_default();     # 2 tests
    test_is_matching_archive_file_matches_in_extension();   # 2 tests
    test_is_matching_archive_file_no_match_in_extension();  # 2 tests
    test_is_matching_archive_file_matches_out_extension();  # 2 tests
    test_is_matching_archive_file_no_match_out_extension(); # 2 tests
    test_is_matching_archive_file_matches_in_pattern();     # 2 tests
    test_is_matching_archive_file_no_match_in_pattern();    # 2 tests
    test_is_matching_archive_file_matches_out_pattern();    # 2 tests
    test_is_matching_archive_file_no_match_out_pattern();   # 2 tests

    # filter_file tests
    test_filter_file_matches_by_default();                  # 2 tests
    test_filter_file_is_matching_file();                    # 2 tests
    test_filter_file_not_is_matching_file();                # 2 tests
    test_filter_file_is_hidden_file();                      # 2 tests
    test_filter_file_hidden_includehidden();                # 2 tests
    test_filter_file_archive_no_include_archives();          # 2 tests
    test_filter_file_archive_include_archives();             # 2 tests
    test_filter_file_archive_archives_only();                # 2 tests
    test_filter_file_nonarchive_archives_only();             # 2 tests
}

main();
