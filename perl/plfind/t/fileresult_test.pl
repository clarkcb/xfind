#!/usr/bin/perl -w
#
# fileresult_test.pl
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

use Test::Simple tests => 2;

use plfind::FileResult;
use plfind::FileType;

sub test_file_result_abs_path {
    my $file_path = file("$lib_path/plfind/FileResult.pm");
    my $file_type = plfind::FileType->CODE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($file_path, $file_type, $file_size, $last_mod);
    my $expected = $file_path->stringify;

    ok($expected eq $file_result->to_string(), "\$file_result->to_string() eq $expected");
    ok(plfind::FileType->CODE eq $file_result->{file_type}, "FileType of $file_path is $file_type");
}

sub main {
    test_file_result_abs_path();
}

main();
