#include <catch2/catch_all.hpp>
#include "FileTypes.h"

TEST_CASE("Verify that files are the expected type", "[FileType]") {
    auto* file_types = new cppfind::FileTypes();
    REQUIRE(file_types->is_archive_path("archive.zip"));
    REQUIRE(file_types->is_audio_path("music.mp3"));
    REQUIRE(file_types->is_binary_path("binary.exe"));
    REQUIRE(file_types->is_code_path("source.cpp"));
    REQUIRE(file_types->is_font_path("font.ttf"));
    REQUIRE(file_types->is_image_path("image.png"));
    REQUIRE(file_types->is_text_path("textfile.txt"));
    REQUIRE(file_types->is_video_path("movie.mp4"));
    REQUIRE(file_types->is_xml_path("markup.xml"));
    REQUIRE(file_types->is_unknown_path("unknown.UNKNOWN"));
}

TEST_CASE("Verify that get_filetype returns the expected type", "[FileType]") {
    auto* file_types = new cppfind::FileTypes();
    REQUIRE(file_types->get_path_type("archive.zip") == cppfind::FileType::ARCHIVE);
    REQUIRE(file_types->get_path_type("music.mp3") == cppfind::FileType::AUDIO);
    REQUIRE(file_types->get_path_type("binary.exe") == cppfind::FileType::BINARY);
    REQUIRE(file_types->get_path_type("source.cpp") == cppfind::FileType::CODE);
    REQUIRE(file_types->get_path_type("font.ttf") == cppfind::FileType::FONT);
    REQUIRE(file_types->get_path_type("image.png") == cppfind::FileType::IMAGE);
    REQUIRE(file_types->get_path_type("textfile.txt") == cppfind::FileType::TEXT);
    REQUIRE(file_types->get_path_type("movie.mp4") == cppfind::FileType::VIDEO);
    REQUIRE(file_types->get_path_type("markup.xml") == cppfind::FileType::XML);
    REQUIRE(file_types->get_path_type("unknown.UNKNOWN") == cppfind::FileType::UNKNOWN);
}
