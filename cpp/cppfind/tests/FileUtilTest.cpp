#include <catch2/catch_all.hpp>
#include <filesystem>

#include "FindConfig.h"
#include "FileUtil.h"

TEST_CASE("Expand path", "[FileUtil]") {
    const auto home_path = std::filesystem::path(getenv("HOME"));
    const auto tilde_path = std::filesystem::path("~");
    const auto tilde_name_path = std::filesystem::path("~cary");
    const std::filesystem::path full_file_path = home_path / "filename.txt";
    const std::filesystem::path tilde_file_path = tilde_path / "filename.txt";
    const std::filesystem::path tilde_name_file_path = tilde_name_path / "filename.txt";
    const std::filesystem::path file_name_path = full_file_path.filename();

    REQUIRE(cppfind::FileUtil::expand_path(home_path) == home_path);
    REQUIRE(cppfind::FileUtil::expand_path(tilde_path) == home_path);
    REQUIRE(cppfind::FileUtil::expand_path(full_file_path) == full_file_path);
    REQUIRE(cppfind::FileUtil::expand_path(tilde_file_path) == full_file_path);
    REQUIRE(cppfind::FileUtil::expand_path(tilde_name_file_path) == full_file_path);
    REQUIRE(cppfind::FileUtil::expand_path(file_name_path) == file_name_path);
}

TEST_CASE("Get extensions from paths", "[FileUtil]") {
    REQUIRE(cppfind::FileUtil::get_path_extension("filename.txt") == "txt");
    REQUIRE(cppfind::FileUtil::get_path_extension("filename.").empty());
    REQUIRE(cppfind::FileUtil::get_path_extension("filename").empty());
    REQUIRE(cppfind::FileUtil::get_path_extension(".filename.txt") == "txt");
    REQUIRE(cppfind::FileUtil::get_path_extension(".filename.").empty());
    REQUIRE(cppfind::FileUtil::get_path_extension(".filename").empty());
}

TEST_CASE("Detect dot dirs", "[FileUtil]") {
    REQUIRE(cppfind::FileUtil::is_dot_dir("."));
    REQUIRE(cppfind::FileUtil::is_dot_dir(".."));
    REQUIRE(cppfind::FileUtil::is_dot_dir("./"));
    REQUIRE(cppfind::FileUtil::is_dot_dir("../"));
    REQUIRE(!cppfind::FileUtil::is_dot_dir("./path"));
    REQUIRE(!cppfind::FileUtil::is_dot_dir("../path"));
    REQUIRE(!cppfind::FileUtil::is_dot_dir(".gitignore"));
}

TEST_CASE("Detect hidden files", "[FileUtil]") {
    REQUIRE(!cppfind::FileUtil::is_hidden("."));
    REQUIRE(!cppfind::FileUtil::is_hidden(".."));
    REQUIRE(!cppfind::FileUtil::is_hidden("./"));
    REQUIRE(!cppfind::FileUtil::is_hidden("../"));
//    REQUIRE(!cppfind::FileUtil::is_hidden("./path"));
//    REQUIRE(!cppfind::FileUtil::is_hidden("../path"));
    REQUIRE(cppfind::FileUtil::is_hidden(".gitignore"));
    REQUIRE(!cppfind::FileUtil::is_hidden("filename.txt"));
}
