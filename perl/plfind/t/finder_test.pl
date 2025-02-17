#!/usr/bin/perl -w
#
# finder_test.pl
#
#
use strict;
use warnings;

use Cwd 'abs_path';
use File::Basename;
use Path::Class;

my $lib_path;

BEGIN {
    $lib_path = dirname(dirname(abs_path($0))) . '/lib';
    # print "lib_path: $lib_path\n";
    unshift @INC, $lib_path;
}

use Test::Simple tests => 79;

use plfind::config;
use plfind::FileUtil;
use plfind::FindSettings;
use plfind::Finder;


sub get_settings {
    my $settings = plfind::FindSettings->new();
    $settings->{paths} = ['.'];
    return $settings;
}

sub get_test_file {
  return "$SHARED_PATH/testFiles/testFile2.txt";
}

sub test_validate_settings {
    my $settings = get_settings();
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
}

################################################################################
# is_matching_dir tests
################################################################################
sub test_is_matching_dir_no_patterns {
    my $settings = get_settings();
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = dir('plfind');
    ok($finder->is_matching_dir($dir), "$dir is matching dir with no patterns");
}

sub test_is_matching_dir_matches_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_dir_patterns}}, 'plfind');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = dir('plfind');
    ok($finder->is_matching_dir($dir), "$dir matches in_dir_patterns");
}

sub test_is_matching_dir_no_match_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_dir_patterns}}, 'plfind');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = dir('pyfind');
    ok(!$finder->is_matching_dir($dir), "$dir does not match in_dir_patterns");
}

sub test_is_matching_dir_matches_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_dir_patterns}}, 'pyfind');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = dir('pyfind');
    ok(!$finder->is_matching_dir($dir), "$dir matches out_dir_patterns");
}

sub test_is_matching_dir_no_match_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_dir_patterns}}, 'pyfind');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = dir('plfind');
    ok($finder->is_matching_dir($dir), "$dir does not match out_dir_patterns");
}

sub test_is_matching_dir_single_dot {
    my $settings = get_settings();
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = dir('.');
    ok($finder->is_matching_dir($dir), "$dir is matching dir");
}

sub test_is_matching_dir_double_dot {
    my $settings = get_settings();
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = dir('..');
    ok($finder->is_matching_dir($dir), "$dir is matching dir");
}

sub test_is_matching_dir_hidden_dir {
    my $settings = get_settings();
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = dir('.git');
    ok(!$finder->is_matching_dir($dir), "Hidden dir $dir is not matching dir by default");
}

sub test_is_matching_dir_hidden_dir_include_hidden {
    my $settings = get_settings();
    $settings->{include_hidden} = 1;
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = dir('.git');
    ok($finder->is_matching_dir($dir),
        "Hidden dir $dir is matching dir with include_hidden set to true");
}

################################################################################
# is_matching_file_result tests
################################################################################
sub test_is_matching_file_matches_by_default {
    my $settings = get_settings();
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./FileUtil.pm');
    my $file_type = plfind::FileType->CODE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok($finder->is_matching_file_result($file_result), "$file_path is matching file by default");
}

sub test_is_matching_file_matches_in_extension {
    my $settings = get_settings();
    push(@{$settings->{in_extensions}}, 'pm');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./FileUtil.pm');
    my $file_type = plfind::FileType->CODE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok($finder->is_matching_file_result($file_result), "$file_path matches in_extensions");
}

sub test_is_matching_file_no_match_in_extension {
    my $settings = get_settings();
    push(@{$settings->{in_extensions}}, 'pl');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./FileUtil.pm');
    my $file_type = plfind::FileType->CODE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok(!$finder->is_matching_file_result($file_result), "$file_path does not match in_extensions");
}

sub test_is_matching_file_matches_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_extensions}}, 'pm');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./FileUtil.pm');
    my $file_type = plfind::FileType->CODE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok(!$finder->is_matching_file_result($file_result), "$file_path matches out_extensions");
}

sub test_is_matching_file_no_match_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_extensions}}, 'py');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./FileUtil.pm');
    my $file_type = plfind::FileType->CODE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok($finder->is_matching_file_result($file_result), "$file_path does not match out_extensions");
}

sub test_is_matching_file_matches_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_file_patterns}}, 'Find');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./Finder.pm');
    my $file_type = plfind::FileType->CODE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok($finder->is_matching_file_result($file_result), "$file_path matches in_file_patterns");
}

sub test_is_matching_file_no_match_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_file_patterns}}, 'Find');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./FileUtil.pm');
    my $file_type = plfind::FileType->CODE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok(!$finder->is_matching_file_result($file_result), "$file_path does not match in_file_patterns");
}

sub test_is_matching_file_matches_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_file_patterns}}, 'Find');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./Finder.pm');
    my $file_type = plfind::FileType->CODE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok(!$finder->is_matching_file_result($file_result), "$file_path matches out_file_patterns");
}

sub test_is_matching_file_no_match_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_file_patterns}}, 'Find');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./FileUtil.pm');
    my $file_type = plfind::FileType->CODE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok($finder->is_matching_file_result($file_result), "$file_path does not match out_file_patterns");
}

################################################################################
# is_matching_archive_file tests
################################################################################
sub test_is_matching_archive_file_matches_by_default {
    my $settings = get_settings();
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./archive.zip');
    my $file_type = plfind::FileType->ARCHIVE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok($finder->is_matching_archive_file_result($file_result), "$file_path is matching archive file by default");
}

sub test_is_matching_archive_file_matches_in_extension {
    my $settings = get_settings();
    push(@{$settings->{in_archive_extensions}}, 'zip');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./archive.zip');
    my $file_type = plfind::FileType->ARCHIVE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok($finder->is_matching_archive_file_result($file_result), "$file_path matches in_archive_extensions");
}

sub test_is_matching_archive_file_no_match_in_extension {
    my $settings = get_settings();
    push(@{$settings->{in_archive_extensions}}, 'gz');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./archive.zip');
    my $file_type = plfind::FileType->ARCHIVE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok(!$finder->is_matching_archive_file_result($file_result), "$file_path does not match in_archive_extensions");
}

sub test_is_matching_archive_file_matches_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_archive_extensions}}, 'zip');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./archive.zip');
    my $file_type = plfind::FileType->ARCHIVE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok(!$finder->is_matching_archive_file_result($file_result), "$file_path matches out_archive_extensions");
}

sub test_is_matching_archive_file_no_match_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_archive_extensions}}, 'gz');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./archive.zip');
    my $file_type = plfind::FileType->ARCHIVE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok($finder->is_matching_archive_file_result($file_result), "$file_path does not match out_archive_extensions");
}

sub test_is_matching_archive_file_matches_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_archive_file_patterns}}, 'arch');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./archive.zip');
    my $file_type = plfind::FileType->ARCHIVE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok($finder->is_matching_archive_file_result($file_result), "$file_path matches in_archive_file_patterns");
}

sub test_is_matching_archive_file_no_match_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_archive_file_patterns}}, 'archives');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./archive.zip');
    my $file_type = plfind::FileType->ARCHIVE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok(!$finder->is_matching_archive_file_result($file_result), "$file_path does not match in_archive_file_patterns");
}

sub test_is_matching_archive_file_matches_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_archive_file_patterns}}, 'arch');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./archive.zip');
    my $file_type = plfind::FileType->ARCHIVE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok(!$finder->is_matching_archive_file_result($file_result), "$file_path matches out_archive_file_patterns");
}

sub test_is_matching_archive_file_no_match_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_archive_file_patterns}}, 'archives');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_path = file('./archive.zip');
    my $file_type = plfind::FileType->ARCHIVE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    ok($finder->is_matching_archive_file_result($file_result), "$file_path does not match out_archive_file_patterns");
}

################################################################################
# filter_to_file_result tests
################################################################################
sub test_filter_file_matches_by_default {
    my $settings = get_settings();
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = file('FileUtil.pm');
    ok(defined $finder->filter_to_file_result($file), "filter_to_file_result($file) defined by default");
}

sub test_filter_file_is_matching_file {
    my $settings = get_settings();
    push(@{$settings->{in_extensions}}, 'pm');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = file('FileUtil.pm');
    ok(defined $finder->filter_to_file_result($file), "filter_to_file_result($file) defined when is_matching_file");
}

sub test_filter_file_not_is_matching_file {
    my $settings = get_settings();
    push(@{$settings->{in_extensions}}, 'pl');
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = file('FileUtil.pm');
    ok(!defined $finder->filter_to_file_result($file), "filter_to_file_result($file) not defined when !is_matching_file");
}

sub test_filter_file_is_hidden_file {
    my $settings = get_settings();
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = file('.gitignore');
    ok(!defined $finder->filter_to_file_result($file), "filter_to_file_result($file) not defined when include_hidden == 0");
}

sub test_filter_file_hidden_includehidden {
    my $settings = get_settings();
    $settings->{include_hidden} = 1;
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = file('.gitignore');
    ok(defined $finder->filter_to_file_result($file), "filter_to_file_result($file) defined when hidden and include_hidden == 1");
}

sub test_filter_file_archive_no_include_archives {
    my $settings = get_settings();
    $settings->{include_archives} = 0;
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = file('archive.zip');
    ok(!defined $finder->filter_to_file_result($file), "filter_to_file_result($file) not defined when include_archives == 0");
}

sub test_filter_file_archive_include_archives {
    my $settings = get_settings();
    $settings->{include_archives} = 1;
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = file('archive.zip');
    ok(defined $finder->filter_to_file_result($file), "filter_to_file_result($file) defined when include_archives == 1");
}

sub test_filter_file_archive_archives_only {
    my $settings = get_settings();
    $settings->{archives_only} = 1;
    $settings->{include_archives} = 1;
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = file('archive.zip');
    ok(defined $finder->filter_to_file_result($file), "filter_to_file_result($file) defined when archives_only=1");
}

sub test_filter_file_nonarchive_archives_only {
    my $settings = get_settings();
    $settings->{archives_only} = 1;
    $settings->{include_archives} = 1;
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = file('FileUtil.pm');
    ok(!defined $finder->filter_to_file_result($file), "filter_to_file_result($file) not defined when archives_only=1");
}

sub test_default_no_symlinks {
    my $settings = plfind::FindSettings->new();
    $settings->{paths} = [dir($XFIND_PATH, 'bin')];
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_results = $finder->find();
    ok(scalar @{$file_results} < 3, "There are less than three file results");
}

sub test_follow_symlinks {
    my $settings = plfind::FindSettings->new();
    $settings->{paths} = [dir($XFIND_PATH, 'bin')];
    $settings->{follow_symlinks} = 1;
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_results = $finder->find();
    ok(scalar @{$file_results} == 0 || scalar @{$file_results} > 2, "There are more than two file results");
}

sub test_no_follow_symlinks {
    my $settings = plfind::FindSettings->new();
    $settings->{paths} = [dir($XFIND_PATH, 'bin')];
    $settings->{follow_symlinks} = 0;
    my ($finder, $errs) = plfind::Finder->new($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file_results = $finder->find();
    ok(scalar @{$file_results} < 3, "There are less than three file results");
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
    test_filter_file_archive_no_include_archives();         # 2 tests
    test_filter_file_archive_include_archives();            # 2 tests
    test_filter_file_archive_archives_only();               # 2 tests
    test_filter_file_nonarchive_archives_only();            # 2 tests

    # test filtering symlink files
    test_default_no_symlinks();                             # 2 tests
    test_follow_symlinks();                                 # 2 tests
    test_no_follow_symlinks();                              # 2 tests

}

main();
