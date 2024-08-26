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
    my $file_name = 'archive.zip';
    my $ok = $file_types->is_archive($file_name);
    ok($ok > 0, "$file_name is archive file");
    my $type = $file_types->get_file_type($file_name);
    ok($type eq plfind::FileType->ARCHIVE, "FileType of $file_name is $type");
}

sub test_get_file_type_audio_file {
    my $file_name = 'music.mp3';
    my $ok = $file_types->is_audio($file_name);
    ok($ok > 0, "$file_name is audio file");
    my $type = $file_types->get_file_type($file_name);
    ok($type eq plfind::FileType->AUDIO, "FileType of $file_name is $type");
}

sub test_get_file_type_binary_file {
    my $file_name = 'binary.exe';
    my $ok = $file_types->is_binary($file_name);
    ok($ok > 0, "$file_name is binary file");
    my $type = $file_types->get_file_type($file_name);
    ok($type eq plfind::FileType->BINARY, "FileType of $file_name is $type");
}

sub test_get_file_type_code_file {
    my $file_name = 'code.pl';
    my $ok = $file_types->is_code($file_name);
    ok($ok > 0, "$file_name is code file");
    my $type = $file_types->get_file_type($file_name);
    ok($type eq plfind::FileType->CODE, "FileType of $file_name is $type");
}

sub test_get_file_type_font_file {
    my $file_name = 'font.ttf';
    my $ok = $file_types->is_font($file_name);
    ok($ok > 0, "$file_name is font file");
    my $type = $file_types->get_file_type($file_name);
    ok($type eq plfind::FileType->FONT, "FileType of $file_name is $type");
}

sub test_get_file_type_image_file {
    my $file_name = 'image.png';
    my $ok = $file_types->is_image($file_name);
    ok($ok > 0, "$file_name is image file");
    my $type = $file_types->get_file_type($file_name);
    ok($type eq plfind::FileType->IMAGE, "FileType of $file_name is $type");
}

sub test_get_file_type_text_file {
    my $file_name = 'text.txt';
    my $ok = $file_types->is_text($file_name);
    ok($ok > 0, "$file_name is text file");
    my $type = $file_types->get_file_type($file_name);
    ok($type eq plfind::FileType->TEXT, "FileType of $file_name is $type");
}

sub test_get_file_type_video_file {
    my $file_name = 'movie.mp4';
    my $ok = $file_types->is_video($file_name);
    ok($ok > 0, "$file_name is video file");
    my $type = $file_types->get_file_type($file_name);
    ok($type eq plfind::FileType->VIDEO, "FileType of $file_name is $type");
}

sub test_get_file_type_xml_file {
    my $file_name = 'markup.xml';
    my $ok = $file_types->is_xml($file_name);
    ok($ok > 0, "$file_name is xml file");
    my $type = $file_types->get_file_type($file_name);
    ok($type eq plfind::FileType->XML, "FileType of $file_name is $type");
}

sub test_get_file_type_unknown_file {
    my $file_name = 'unknown.xyz';
    my $ok = $file_types->is_unknown($file_name);
    ok($ok > 0, "$file_name is unknown file");
    my $type = $file_types->get_file_type($file_name);
    ok($type eq plfind::FileType->UNKNOWN, "FileType of $file_name is $type");
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
