#!/usr/bin/perl -w
#
# filetypes_test.pl
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

use Test::Simple tests => 20;

use plfind::FileType;
use plfind::FileTypes;

my $file_types = plfind::FileTypes->new();

sub test_get_file_type_archive_file {
    my $filename = 'archive.zip';
    my $ok = $file_types->is_archive($filename);
    ok($ok > 0, "$filename is archive file");
    my $type = $file_types->get_file_type($filename);
    ok($type eq plfind::FileType->ARCHIVE, "FileType of $filename is $type");
}

sub test_get_file_type_audio_file {
    my $filename = 'music.mp3';
    my $ok = $file_types->is_audio($filename);
    ok($ok > 0, "$filename is audio file");
    my $type = $file_types->get_file_type($filename);
    ok($type eq plfind::FileType->AUDIO, "FileType of $filename is $type");
}

sub test_get_file_type_binary_file {
    my $filename = 'binary.exe';
    my $ok = $file_types->is_binary($filename);
    ok($ok > 0, "$filename is binary file");
    my $type = $file_types->get_file_type($filename);
    ok($type eq plfind::FileType->BINARY, "FileType of $filename is $type");
}

sub test_get_file_type_code_file {
    my $filename = 'code.pl';
    my $ok = $file_types->is_code($filename);
    ok($ok > 0, "$filename is code file");
    my $type = $file_types->get_file_type($filename);
    ok($type eq plfind::FileType->CODE, "FileType of $filename is $type");
}

sub test_get_file_type_font_file {
    my $filename = 'font.ttf';
    my $ok = $file_types->is_font($filename);
    ok($ok > 0, "$filename is font file");
    my $type = $file_types->get_file_type($filename);
    ok($type eq plfind::FileType->FONT, "FileType of $filename is $type");
}

sub test_get_file_type_image_file {
    my $filename = 'image.png';
    my $ok = $file_types->is_image($filename);
    ok($ok > 0, "$filename is image file");
    my $type = $file_types->get_file_type($filename);
    ok($type eq plfind::FileType->IMAGE, "FileType of $filename is $type");
}

sub test_get_file_type_text_file {
    my $filename = 'text.txt';
    my $ok = $file_types->is_text($filename);
    ok($ok > 0, "$filename is text file");
    my $type = $file_types->get_file_type($filename);
    ok($type eq plfind::FileType->TEXT, "FileType of $filename is $type");
}

sub test_get_file_type_video_file {
    my $filename = 'movie.mp4';
    my $ok = $file_types->is_video($filename);
    ok($ok > 0, "$filename is video file");
    my $type = $file_types->get_file_type($filename);
    ok($type eq plfind::FileType->VIDEO, "FileType of $filename is $type");
}

sub test_get_file_type_xml_file {
    my $filename = 'markup.xml';
    my $ok = $file_types->is_xml($filename);
    ok($ok > 0, "$filename is xml file");
    my $type = $file_types->get_file_type($filename);
    ok($type eq plfind::FileType->XML, "FileType of $filename is $type");
}

sub test_get_file_type_unknown_file {
    my $filename = 'unknown.xyz';
    my $ok = $file_types->is_unknown($filename);
    ok($ok > 0, "$filename is unknown file");
    my $type = $file_types->get_file_type($filename);
    ok($type eq plfind::FileType->UNKNOWN, "FileType of $filename is $type");
}

sub main {
    test_get_file_type_archive_file();
    test_get_file_type_audio_file();
    test_get_file_type_binary_file();
    test_get_file_type_code_file();
    test_get_file_type_font_file();
    test_get_file_type_image_file();
    test_get_file_type_text_file();
    test_get_file_type_video_file();
    test_get_file_type_xml_file();
    test_get_file_type_unknown_file();
}

main();
