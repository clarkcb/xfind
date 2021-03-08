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
# is_find_dir tests
################################################################################
sub test_is_find_dir_no_patterns {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'plfind';
    ok($finder->is_find_dir($dir), "$dir is find dir with no patterns");
}

sub test_is_find_dir_matches_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_dirpatterns}}, 'plfind');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'plfind';
    ok($finder->is_find_dir($dir), "$dir matches in_dirpatterns");
}

sub test_is_find_dir_no_match_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_dirpatterns}}, 'plfind');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'pyfind';
    ok(!$finder->is_find_dir($dir), "$dir does not match in_dirpatterns");
}

sub test_is_find_dir_matches_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_dirpatterns}}, 'pyfind');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'pyfind';
    ok(!$finder->is_find_dir($dir), "$dir matches out_dirpatterns");
}

sub test_is_find_dir_no_match_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_dirpatterns}}, 'pyfind');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = 'plfind';
    ok($finder->is_find_dir($dir), "$dir does not match out_dirpatterns");
}

sub test_is_find_dir_single_dot {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = '.';
    ok($finder->is_find_dir($dir), "$dir is find dir");
}

sub test_is_find_dir_double_dot {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = '..';
    ok($finder->is_find_dir($dir), "$dir is find dir");
}

sub test_is_find_dir_hidden_dir {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = '.git';
    ok(!$finder->is_find_dir($dir), "Hidden dir $dir is not find dir by default");
}

sub test_is_find_dir_hidden_dir_include_hidden {
    my $settings = get_settings();
    $settings->{excludehidden} = 0;
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $dir = '.git';
    ok($finder->is_find_dir($dir),
        "Hidden dir $dir is find dir with excludehidden set to false");
}

################################################################################
# is_find_file tests
################################################################################
sub test_is_find_file_matches_by_default {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok($finder->is_find_file($file), "$file is find file by default");
}

sub test_is_find_file_matches_in_extension {
    my $settings = get_settings();
    push(@{$settings->{in_extensions}}, 'pm');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok($finder->is_find_file($file), "$file matches in_extensions");
}

sub test_is_find_file_no_match_in_extension {
    my $settings = get_settings();
    push(@{$settings->{in_extensions}}, 'pl');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok(!$finder->is_find_file($file), "$file does not match in_extensions");
}

sub test_is_find_file_matches_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_extensions}}, 'pm');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok(!$finder->is_find_file($file), "$file matches out_extensions");
}

sub test_is_find_file_no_match_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_extensions}}, 'py');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok($finder->is_find_file($file), "$file does not match out_extensions");
}

sub test_is_find_file_matches_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_filepatterns}}, 'Find');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'Finder.pm';
    ok($finder->is_find_file($file), "$file matches in_filepatterns");
}

sub test_is_find_file_no_match_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_filepatterns}}, 'Find');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok(!$finder->is_find_file($file), "$file does not match in_filepatterns");
}

sub test_is_find_file_matches_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_filepatterns}}, 'Find');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'Finder.pm';
    ok(!$finder->is_find_file($file), "$file matches out_filepatterns");
}

sub test_is_find_file_no_match_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_filepatterns}}, 'Find');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok($finder->is_find_file($file), "$file does not match out_filepatterns");
}

################################################################################
# is__archive_find_file tests
################################################################################
sub test_is_archive_find_file_matches_by_default {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($finder->is_archive_find_file($file), "$file is archive find file by default");
}

sub test_is_archive_find_file_matches_in_extension {
    my $settings = get_settings();
    push(@{$settings->{in_archiveextensions}}, 'zip');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($finder->is_archive_find_file($file), "$file matches in_archiveextensions");
}

sub test_is_archive_find_file_no_match_in_extension {
    my $settings = get_settings();
    push(@{$settings->{in_archiveextensions}}, 'gz');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(!$finder->is_archive_find_file($file), "$file does not match in_archiveextensions");
}

sub test_is_archive_find_file_matches_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_archiveextensions}}, 'zip');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(!$finder->is_archive_find_file($file), "$file matches out_archiveextensions");
}

sub test_is_archive_find_file_no_match_out_extension {
    my $settings = get_settings();
    push(@{$settings->{out_archiveextensions}}, 'gz');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($finder->is_archive_find_file($file), "$file does not match out_archiveextensions");
}

sub test_is_archive_find_file_matches_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_archivefilepatterns}}, 'arch');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($finder->is_archive_find_file($file), "$file matches in_archivefilepatterns");
}

sub test_is_archive_find_file_no_match_in_pattern {
    my $settings = get_settings();
    push(@{$settings->{in_archivefilepatterns}}, 'archives');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(!$finder->is_archive_find_file($file), "$file does not match in_archivefilepatterns");
}

sub test_is_archive_find_file_matches_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_archivefilepatterns}}, 'arch');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok(!$finder->is_archive_find_file($file), "$file matches out_archivefilepatterns");
}

sub test_is_archive_find_file_no_match_out_pattern {
    my $settings = get_settings();
    push(@{$settings->{out_archivefilepatterns}}, 'archives');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    ok($finder->is_archive_find_file($file), "$file does not match out_archivefilepatterns");
}

################################################################################
# filter_file tests
################################################################################
sub test_filter_file_matches_by_default {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok($finder->filter_file($file), "$file passes filter_file by default");
}

sub test_filter_file_is_find_file {
    my $settings = get_settings();
    push(@{$settings->{in_extensions}}, 'pm');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok($finder->filter_file($file), "$file passes filter_file when is_find_file");
}

sub test_filter_file_not_is_find_file {
    my $settings = get_settings();
    push(@{$settings->{in_extensions}}, 'pl');
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    ok(!$finder->filter_file($file), "$file does not pass filter_file when !is_find_file");
}

sub test_filter_file_is_hidden_file {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = '.gitignore';
    ok(!$finder->filter_file($file), "$file does not pass filter_file when excludehidden=1");
}

sub test_filter_file_hidden_includehidden {
    my $settings = get_settings();
    $settings->{excludehidden} = 0;
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = '.gitignore';
    ok($finder->filter_file($file), "$file passes filter_file when hidden and excludehidden=0");
}

sub test_filter_file_archive_no_includearchives {
    my $settings = get_settings();
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    #print "finder->is_archive_find_file(archive.zip): " . $finder->is_archive_find_file('archive.zip') . "\n";
    ok(!$finder->filter_file($file), "$file does not pass filter_file when includearchives=0");
}

sub test_filter_file_archive_includearchives {
    my $settings = get_settings();
    $settings->{includearchives} = 1;
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    #print "finder->is_archive_find_file(archive.zip): " . $finder->is_archive_find_file('archive.zip') . "\n";
    ok($finder->filter_file($file), "$file passes filter_file when includearchives=1");
}

sub test_filter_file_archive_archivesonly {
    my $settings = get_settings();
    $settings->{archivesonly} = 1;
    $settings->{includearchives} = 1;
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'archive.zip';
    #print "finder->is_archive_find_file(archive.zip): " . $finder->is_archive_find_file('archive.zip') . "\n";
    ok($finder->filter_file($file), "$file passes filter_file when archivesonly=1");
}

sub test_filter_file_nonarchive_archivesonly {
    my $settings = get_settings();
    $settings->{archivesonly} = 1;
    $settings->{includearchives} = 1;
    my ($finder, $errs) = new plfind::Finder($settings);
    ok(scalar @{$errs} == 0, 'No errors from valid settings');
    my $file = 'FileUtil.pm';
    #print "finder->is_archive_find_file(archive.zip): " . $finder->is_archive_find_file('archive.zip') . "\n";
    ok(!$finder->filter_file($file), "$file does not pass filter_file when archivesonly=1");
}

################################################################################
# main
################################################################################
sub main {

    test_validate_settings();                             # 1 test

    # is_find_dir tests
    test_is_find_dir_no_patterns();                     # 2 tests
    test_is_find_dir_matches_in_pattern();              # 2 tests
    test_is_find_dir_no_match_in_pattern();             # 2 tests
    test_is_find_dir_matches_out_pattern();             # 2 tests
    test_is_find_dir_no_match_out_pattern();            # 2 tests
    test_is_find_dir_single_dot();                      # 2 tests
    test_is_find_dir_double_dot();                      # 2 tests
    test_is_find_dir_hidden_dir();                      # 2 tests
    test_is_find_dir_hidden_dir_include_hidden();       # 2 tests

    # is_find_file tests
    test_is_find_file_matches_by_default();             # 2 tests
    test_is_find_file_matches_in_extension();           # 2 tests
    test_is_find_file_no_match_in_extension();          # 2 tests
    test_is_find_file_matches_out_extension();          # 2 tests
    test_is_find_file_no_match_out_extension();         # 2 tests
    test_is_find_file_matches_in_pattern();             # 2 tests
    test_is_find_file_no_match_in_pattern();            # 2 tests
    test_is_find_file_matches_out_pattern();            # 2 tests
    test_is_find_file_no_match_out_pattern();           # 2 tests

    # is_archive_find_file tests
    test_is_archive_find_file_matches_by_default();     # 2 tests
    test_is_archive_find_file_matches_in_extension();   # 2 tests
    test_is_archive_find_file_no_match_in_extension();  # 2 tests
    test_is_archive_find_file_matches_out_extension();  # 2 tests
    test_is_archive_find_file_no_match_out_extension(); # 2 tests
    test_is_archive_find_file_matches_in_pattern();     # 2 tests
    test_is_archive_find_file_no_match_in_pattern();    # 2 tests
    test_is_archive_find_file_matches_out_pattern();    # 2 tests
    test_is_archive_find_file_no_match_out_pattern();   # 2 tests

    # filter_file tests
    test_filter_file_matches_by_default();                # 2 tests
    test_filter_file_is_find_file();                    # 2 tests
    test_filter_file_not_is_find_file();                # 2 tests
    test_filter_file_is_hidden_file();                    # 2 tests
    test_filter_file_hidden_includehidden();              # 2 tests
    test_filter_file_archive_no_includearchives();         # 2 tests
    test_filter_file_archive_includearchives();            # 2 tests
    test_filter_file_archive_archivesonly();              # 2 tests
    test_filter_file_nonarchive_archivesonly();           # 2 tests
}

main();
