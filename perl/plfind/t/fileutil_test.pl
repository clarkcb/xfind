#!/usr/bin/perl -w
#
# fileutil_test.pl
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

use Test::Simple tests => 16;

use plfind::FileUtil;

################################################################################
# expand_path tests
################################################################################
sub test_expand_path_tilde {
    my $path = '~';
    my $expected = $ENV{HOME};
    my $expanded = plfind::FileUtil::expand_path($path);
    ok($expanded eq $expected, "$path expands to home path");
}

sub test_expand_path_with_tilde {
    my $path = '~/src/xfind';
    my $expected = $ENV{HOME} . '/src/xfind';
    my $expanded = plfind::FileUtil::expand_path($path);
    ok($expanded eq $expected, "$path expands to start with home path");
}

sub test_expand_path_with_tilde_and_name {
    my $path = '~cary/src/xfind';
    my $expected = $ENV{HOME} . '/src/xfind';
    my $expanded = plfind::FileUtil::expand_path($path);
    ok($expanded eq $expected, "$path expands to start with home path");
}

################################################################################
# get_extension tests
################################################################################
sub test_get_extension_has_txt_extension {
    my $file_name = 'filename.txt';
    my $ext = plfind::FileUtil::get_extension($file_name);
    ok($ext eq "txt", "$file_name has extension txt");
}

sub test_get_extension_missing_extension {
    my $file_name = 'filename.';
    my $ext = plfind::FileUtil::get_extension($file_name);
    ok($ext eq "", "$file_name has missing extension");
}

sub test_get_extension_no_extension {
    my $file_name = 'filename';
    my $ext = plfind::FileUtil::get_extension($file_name);
    ok($ext eq "", "$file_name has no extension");
}

sub test_get_extension_hidden_txt_extension {
    my $file_name = '.filename.txt';
    my $ext = plfind::FileUtil::get_extension($file_name);
    ok($ext eq "txt", "$file_name has extension txt");
}

sub test_get_extension_hidden_missing_extension {
    my $file_name = '.filename.';
    my $ext = plfind::FileUtil::get_extension($file_name);
    ok($ext eq "", "$file_name has missing extension");
}

sub test_get_extension_hidden_no_extension {
    my $file_name = '.filename';
    my $ext = plfind::FileUtil::get_extension($file_name);
    ok($ext eq "", "$file_name has no extension");
}

################################################################################
# is_dot_dir tests
################################################################################
sub test_is_dot_dir_single_dot {
    my $file_name = '.';
    my $ok = plfind::FileUtil::is_dot_dir($file_name);
    ok($ok > 0, "$file_name is dot dir");
}

sub test_is_dot_dir_double_dot {
    my $file_name = '..';
    my $ok = plfind::FileUtil::is_dot_dir($file_name);
    ok($ok > 0, "$file_name is dot dir");
}

sub test_is_dot_dir_non_dot_dir {
    my $file_name = '.git';
    my $ok = plfind::FileUtil::is_dot_dir($file_name);
    ok($ok == 0, "$file_name is not dot dir");
}

################################################################################
# is_hidden tests
################################################################################
sub test_is_hidden_hidden_file {
    my $file_name = '.filename.txt';
    my $ok = plfind::FileUtil::is_hidden($file_name);
    ok($ok > 0, "$file_name is hidden file");
}

sub test_is_hidden_not_hidden_file {
    my $file_name = 'filename.txt';
    my $ok = plfind::FileUtil::is_hidden($file_name);
    ok($ok == 0, "$file_name is not hidden file");
}

sub test_is_hidden_single_dot {
    my $file_name = '.';
    my $ok = plfind::FileUtil::is_hidden($file_name);
    ok($ok == 0, "$file_name is not hidden file");
}

sub test_is_hidden_double_dot {
    my $file_name = '..';
    my $ok = plfind::FileUtil::is_hidden($file_name);
    ok($ok == 0, "$file_name is not hidden file");
}

################################################################################
# main
################################################################################
sub main {
    test_expand_path_tilde();
    test_expand_path_with_tilde();
    test_expand_path_with_tilde_and_name();
    test_get_extension_has_txt_extension();
    test_get_extension_missing_extension();
    test_get_extension_no_extension();
    test_get_extension_hidden_txt_extension();
    test_get_extension_hidden_missing_extension();
    test_get_extension_hidden_no_extension();
    test_is_dot_dir_single_dot();
    test_is_dot_dir_double_dot();
    test_is_dot_dir_non_dot_dir();
    test_is_hidden_hidden_file();
    test_is_hidden_not_hidden_file();
    test_is_hidden_single_dot();
    test_is_hidden_double_dot();
}

main();
