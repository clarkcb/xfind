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

sub test_fileresult_abspath {
    my $pathname = "$lib_path/plfind";
    my $filename = 'FileResult.pm';
    my $filetype = plfind::FileType->CODE;
    my $stat = undef;
    my $fileresult = new plfind::FileResult($pathname, $filename, $filetype, $stat);
    my $expected = "$pathname/FileResult.pm";

    ok($expected eq $fileresult->to_string(), "\$fileresult->to_string() eq $expected");
    ok(plfind::FileType->CODE eq $fileresult->{filetype}, "FileType of $filename is $filetype");
}

sub main {
    test_fileresult_abspath();
}

main();
