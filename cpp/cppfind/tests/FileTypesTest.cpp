#include <catch2/catch.hpp>
#include "FileTypes.h"

TEST_CASE("Verify that files are the expected type", "[FileType]") {
    auto* file_types = new cppfind::FileTypes();
    REQUIRE(file_types->is_archive_file("archive.zip"));
    REQUIRE(file_types->is_binary_file("binary.exe"));
    REQUIRE(file_types->is_text_file("textfile.txt"));
    REQUIRE(file_types->is_code_file("source.cpp"));
    REQUIRE(file_types->is_xml_file("markup.xml"));
    REQUIRE(file_types->is_unknown_file("unknown.UNKNOWN"));
}

TEST_CASE("Verify that get_filetype returns the expected type", "[FileType]") {
    auto* file_types = new cppfind::FileTypes();
    REQUIRE(file_types->get_file_type("archive.zip") == cppfind::FileType::ARCHIVE);
    REQUIRE(file_types->get_file_type("binary.exe") == cppfind::FileType::BINARY);
    REQUIRE(file_types->get_file_type("textfile.txt") == cppfind::FileType::TEXT);
    REQUIRE(file_types->get_file_type("source.cpp") == cppfind::FileType::CODE);
    REQUIRE(file_types->get_file_type("markup.xml") == cppfind::FileType::XML);
    REQUIRE(file_types->get_file_type("unknown.UNKNOWN") == cppfind::FileType::UNKNOWN);
}
