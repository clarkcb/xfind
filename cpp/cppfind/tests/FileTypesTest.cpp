#include <catch2/catch.hpp>
#include "FileTypes.h"

TEST_CASE("Verify that files are the expected type", "[FileType]") {
    auto* filetypes = new cppfind::FileTypes();
    REQUIRE(filetypes->is_archive_file("archive.zip"));
    REQUIRE(filetypes->is_binary_file("binary.exe"));
    REQUIRE(filetypes->is_text_file("textfile.txt"));
    REQUIRE(filetypes->is_code_file("source.cpp"));
    REQUIRE(filetypes->is_xml_file("markup.xml"));
    REQUIRE(filetypes->is_unknown_file("unknown.UNKNOWN"));
}

TEST_CASE("Verify that get_filetype returns the expected type", "[FileType]") {
    auto* filetypes = new cppfind::FileTypes();
    REQUIRE(filetypes->get_filetype("archive.zip") == cppfind::FileType::ARCHIVE);
    REQUIRE(filetypes->get_filetype("binary.exe") == cppfind::FileType::BINARY);
    REQUIRE(filetypes->get_filetype("textfile.txt") == cppfind::FileType::TEXT);
    REQUIRE(filetypes->get_filetype("source.cpp") == cppfind::FileType::CODE);
    REQUIRE(filetypes->get_filetype("markup.xml") == cppfind::FileType::XML);
    REQUIRE(filetypes->get_filetype("unknown.UNKNOWN") == cppfind::FileType::UNKNOWN);
}
