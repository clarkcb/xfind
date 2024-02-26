#include <catch2/catch_all.hpp>
#include "FileResult.h"

TEST_CASE("Verify full path file result string equals expected", "[FileResult]") {
    std::filesystem::path file_path{"~/src/xfind/cpp/cppfind/src/Finder.cpp"};
    auto file_type = cppfind::FileType::CODE;
    uint64_t file_size = 1000;
    long mod_time = 1000;
    auto fr = cppfind::FileResult(std::move(file_path), file_type, file_size, mod_time);

    REQUIRE(fr.file_path().parent_path().string() == "~/src/xfind/cpp/cppfind/src");
    REQUIRE(fr.file_path().filename().string() == "Finder.cpp");
    REQUIRE(fr.file_type() == cppfind::FileType::CODE);
    REQUIRE(fr.file_size() == file_size);
    REQUIRE(fr.mod_time() == mod_time);
    REQUIRE(fr.string() == "~/src/xfind/cpp/cppfind/src/Finder.cpp");
}

TEST_CASE("Verify relative path file result string equals expected", "[FileResult]") {
    std::filesystem::path file_path = "Finder.cpp";
    auto file_type = cppfind::FileType::CODE;
    uint64_t file_size = 1000;
    long mod_time = 1000;
    auto fr = cppfind::FileResult(std::move(file_path), file_type, file_size, mod_time);

    REQUIRE(fr.file_path().parent_path().empty());
    REQUIRE(fr.file_path().filename().string() == "Finder.cpp");
    REQUIRE(fr.file_type() == cppfind::FileType::CODE);
    REQUIRE(fr.file_size() == file_size);
    REQUIRE(fr.mod_time() == mod_time);
    REQUIRE(fr.string() == "Finder.cpp");
}
