#include <catch2/catch.hpp>
#include "FindFile.h"

TEST_CASE("Verify fullpath findfile string equals expected", "[FindFile]") {
    std::string path = "/Users/cary/src/xfind/cpp/cppfind/src";
    std::string filename = "Finder.cpp";
    auto *findfile = new cppfind::FindFile(path, filename, cppfind::FileType::CODE);

    REQUIRE(findfile->path() == "/Users/cary/src/xfind/cpp/cppfind/src");
    REQUIRE(findfile->filename() == "Finder.cpp");
    REQUIRE(findfile->filetype() == cppfind::FileType::CODE);
    REQUIRE(findfile->string() == "/Users/cary/src/xfind/cpp/cppfind/src/Finder.cpp");
}

TEST_CASE("Verify relative path findfile string equals expected", "[FindFile]") {
    std::string path = ".";
    std::string filename = "Finder.cpp";
    auto *findfile = new cppfind::FindFile(path, filename, cppfind::FileType::CODE);

    REQUIRE(findfile->path() == ".");
    REQUIRE(findfile->filename() == "Finder.cpp");
    REQUIRE(findfile->filetype() == cppfind::FileType::CODE);
    REQUIRE(findfile->string() == "./Finder.cpp");
}
