#!/bin/bash
################################################################################
#
# test_file_types.bash
#
# Unit tests for file_types functions
#
################################################################################

TESTDIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

source "$TESTDIR/test_common.bash"

test_is_file_type () {
    echo
    hdr "test_is_file_type"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    archive_file="archive.zip"
    is_file_type archive "$archive_file"
    assert_equals_number "is_file_type archive $archive_file" $? 1

    audio_file="audio.mp3"
    is_file_type audio "$audio_file"
    assert_equals_number "is_file_type audio $audio_file" $? 1

    bin_file="binary.exe"
    is_file_type binary "$bin_file"
    assert_equals_number "is_file_type binary $bin_file" $? 1

    code_file="code.bash"
    is_file_type code "$code_file"
    assert_equals_number "is_file_type code $code_file" $? 1

    code_file_no_ext="Makefile"
    is_file_type code "$code_file_no_ext"
    assert_equals_number "is_file_type code $code_file_no_ext" $? 1

    font_file="font.ttf"
    is_file_type font "$font_file"
    assert_equals_number "is_file_type font $font_file" $? 1

    image_file="image.png"
    is_file_type image "$image_file"
    assert_equals_number "is_file_type image $image_file" $? 1

    text_file="test.txt"
    is_file_type text "$text_file"
    assert_equals_number "is_file_type text $text_file" $? 1

    video_file="movie.mp4"
    is_file_type video "$video_file"
    assert_equals_number "is_file_type video $video_file" $? 1

    xml_file="pom.xml"
    is_file_type xml "$xml_file"
    assert_equals_number "is_file_type xml $xml_file" $? 1
}

test_get_file_type () {
    echo
    hdr "test_get_file_type"

    source "$BASHFIND_PATH/lib/bashfindlib.bash"

    archive_file="archive.zip"
    archive_file_type=$(get_file_type "$archive_file")
    assert_equals_string "get_file_type $archive_file" "$archive_file_type" archive

    audio_file="audio.mp3"
    audio_file_type=$(get_file_type "$audio_file")
    assert_equals_string "get_file_type $audio_file" "$audio_file_type" audio

    bin_file="binary.exe"
    bin_file_type=$(get_file_type "$bin_file")
    assert_equals_string "get_file_type $bin_file" "$bin_file_type" binary

    code_file="code.bash"
    code_file_type=$(get_file_type "$code_file")
    assert_equals_string "get_file_type $code_file" "$code_file_type" code

    code_file_no_ext="Makefile"
    code_file_not_ext_type=$(get_file_type "$code_file_no_ext")
    assert_equals_string "get_file_type $code_file_no_ext" "$code_file_not_ext_type" code

    font_file="font.ttf"
    font_file_type=$(get_file_type "$font_file")
    assert_equals_string "get_file_type $font_file" "$font_file_type" font

    image_file="image.png"
    image_file_type=$(get_file_type "$image_file")
    assert_equals_string "get_file_type $image_file" "$image_file_type" image

    text_file="test.txt"
    text_file_type=$(get_file_type "$text_file")
    assert_equals_string "get_file_type $text_file" "$text_file_type" text

    video_file="movie.mp4"
    video_file_type=$(get_file_type "$video_file")
    assert_equals_string "get_file_type $video_file" "$video_file_type" video

    xml_file="pom.xml"
    xml_file_type=$(get_file_type "$xml_file")
    assert_equals_string "get_file_type $xml_file" "$xml_file_type" xml
}
