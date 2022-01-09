#include <catch2/catch.hpp>
#include "FileResult.h"

TEST_CASE("Verify fullpath findfile string equals expected", "[FileResult]") {
    std::string path = "~/src/xfind/cpp/cppfind/src";
    std::string filename = "Finder.cpp";
    auto *findfile = new cppfind::FileResult(path, filename, cppfind::FileType::CODE);

    REQUIRE(findfile->path() == "~/src/xfind/cpp/cppfind/src");
    REQUIRE(findfile->filename() == "Finder.cpp");
    REQUIRE(findfile->filetype() == cppfind::FileType::CODE);
    REQUIRE(findfile->string() == "~/src/xfind/cpp/cppfind/src/Finder.cpp");
}

TEST_CASE("Verify relative path findfile string equals expected", "[FileResult]") {
    std::string path = ".";
    std::string filename = "Finder.cpp";
    auto *findfile = new cppfind::FileResult(path, filename, cppfind::FileType::CODE);

    REQUIRE(findfile->path() == ".");
    REQUIRE(findfile->filename() == "Finder.cpp");
    REQUIRE(findfile->filetype() == cppfind::FileType::CODE);
    REQUIRE(findfile->string() == "./Finder.cpp");
}
