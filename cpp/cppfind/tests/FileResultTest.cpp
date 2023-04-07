#include <catch2/catch.hpp>
#include "FileResult.h"

TEST_CASE("Verify fullpath findfile string equals expected", "[FileResult]") {
    std::string path = "~/src/xfind/cpp/cppfind/src";
    std::string filename = "Finder.cpp";
    uint64_t filesize = 1000;
    long modtime = 1000;
    auto *fr = new cppfind::FileResult(path, filename, cppfind::FileType::CODE, filesize, modtime);

    REQUIRE(fr->path() == "~/src/xfind/cpp/cppfind/src");
    REQUIRE(fr->filename() == "Finder.cpp");
    REQUIRE(fr->filetype() == cppfind::FileType::CODE);
    REQUIRE(fr->filesize() == 1000);
    REQUIRE(fr->modtime() == 1000);
    REQUIRE(fr->string() == "~/src/xfind/cpp/cppfind/src/Finder.cpp");
}

TEST_CASE("Verify relative path findfile string equals expected", "[FileResult]") {
    std::string path = ".";
    std::string filename = "Finder.cpp";
    uint64_t filesize = 1000;
    long modtime = 1000;
    auto *fr = new cppfind::FileResult(path, filename, cppfind::FileType::CODE, filesize, modtime);

    REQUIRE(fr->path() == ".");
    REQUIRE(fr->filename() == "Finder.cpp");
    REQUIRE(fr->filetype() == cppfind::FileType::CODE);
    REQUIRE(fr->filesize() == 1000);
    REQUIRE(fr->modtime() == 1000);
    REQUIRE(fr->string() == "./Finder.cpp");
}
