#!/usr/bin/perl -w
#
# fileresult_test.pl
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

use Test::Simple tests => 2;

use plfind::FileResult;
use plfind::FileType;
# use plfind::FileTypes;

sub test_file_result_abspath {
    my $path = "$lib_path/plfind";
    my $file_name = 'FileResult.pm';
    my $file_type = plfind::FileType->CODE;
    my $file_size = 0;
    my $last_mod = 0;
    my $file_result = plfind::FileResult->new($path, $file_name, $file_type, $file_size, $last_mod);
    my $expected = "$path/FileResult.pm";

    ok($expected eq $file_result->to_string(), "\$file_result->to_string() eq $expected");
    ok(plfind::FileType->CODE eq $file_result->{file_type}, "FileType of $file_name is $file_type");
}

sub main {
    test_file_result_abspath();
}

main();
