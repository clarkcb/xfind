#include <catch2/catch.hpp>
#include "FileResult.h"

TEST_CASE("Verify full path file result string equals expected", "[FileResult]") {
    std::string path = "~/src/xfind/cpp/cppfind/src";
    std::string file_name = "Finder.cpp";
    cppfind::FileType file_type = cppfind::FileType::CODE;
    uint64_t file_size = 1000;
    long mod_time = 1000;
    auto *fr = new cppfind::FileResult(path, file_name, file_type, file_size, mod_time);

    REQUIRE(fr->path() == "~/src/xfind/cpp/cppfind/src");
    REQUIRE(fr->file_name() == "Finder.cpp");
    REQUIRE(fr->file_type() == cppfind::FileType::CODE);
    REQUIRE(fr->file_size() == file_size);
    REQUIRE(fr->mod_time() == mod_time);
    REQUIRE(fr->string() == "~/src/xfind/cpp/cppfind/src/Finder.cpp");
}

TEST_CASE("Verify relative path file result string equals expected", "[FileResult]") {
    std::string path = ".";
    std::string file_name = "Finder.cpp";
    cppfind::FileType file_type = cppfind::FileType::CODE;
    uint64_t file_size = 1000;
    long mod_time = 1000;
    auto *fr = new cppfind::FileResult(path, file_name, file_type, file_size, mod_time);

    REQUIRE(fr->path() == ".");
    REQUIRE(fr->file_name() == "Finder.cpp");
    REQUIRE(fr->file_type() == cppfind::FileType::CODE);
    REQUIRE(fr->file_size() == file_size);
    REQUIRE(fr->mod_time() == mod_time);
    REQUIRE(fr->string() == "./Finder.cpp");
}
