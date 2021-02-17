#include <config.h>
#include <catch2/catch.hpp>
#include "FileUtil.h"

TEST_CASE("Expand paths", "[FileUtil]") {
    REQUIRE(cppfind::FileUtil::expand_path("filename.txt") == "filename.txt");
    REQUIRE(cppfind::FileUtil::expand_path("./filename.txt") == "./filename.txt");
    REQUIRE(cppfind::FileUtil::expand_path("/Users/cary/filename.txt") == "/Users/cary/filename.txt");
    REQUIRE(cppfind::FileUtil::expand_path("~/filename.txt") == "/Users/cary/filename.txt");
    REQUIRE(cppfind::FileUtil::expand_path("~") == "/Users/cary");
    REQUIRE(cppfind::FileUtil::expand_path("~/") == "/Users/cary/");
}

TEST_CASE("Detect file existence", "[FileUtil]") {
    REQUIRE(cppfind::FileUtil::file_exists(std::string(XFINDPATH) + "/shared/config.json"));
    REQUIRE(!cppfind::FileUtil::file_exists(std::string(XFINDPATH) + "/nonexistant.txt"));
}

TEST_CASE("Get extensions from filenames", "[FileUtil]") {
    REQUIRE(cppfind::FileUtil::get_extension("filename.txt") == "txt");
    REQUIRE(cppfind::FileUtil::get_extension("filename.").empty());
    REQUIRE(cppfind::FileUtil::get_extension("filename").empty());
    REQUIRE(cppfind::FileUtil::get_extension(".filename.txt") == "txt");
    REQUIRE(cppfind::FileUtil::get_extension(".filename.").empty());
    REQUIRE(cppfind::FileUtil::get_extension(".filename").empty());
}

TEST_CASE("Detect directories", "[FileUtil]") {
    REQUIRE(cppfind::FileUtil::is_directory("."));
    REQUIRE(cppfind::FileUtil::is_directory(".."));
    REQUIRE(cppfind::FileUtil::is_directory("./"));
    REQUIRE(cppfind::FileUtil::is_directory("../"));
    REQUIRE(cppfind::FileUtil::is_directory(std::string(XFINDPATH)));
    REQUIRE(!cppfind::FileUtil::is_directory(std::string(XFINDPATH) + "/shared/config.json"));
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

TEST_CASE("Split paths", "[FileUtil]") {
    REQUIRE(cppfind::FileUtil::split_path(".").size() == 1);
    REQUIRE(cppfind::FileUtil::split_path(".")[0] == ".");
    REQUIRE(cppfind::FileUtil::split_path("./").size() == 1);
    REQUIRE(cppfind::FileUtil::split_path("./")[0] == ".");
    REQUIRE(cppfind::FileUtil::split_path("./path").size() == 2);
    REQUIRE(cppfind::FileUtil::split_path("./path")[0] == ".");
    REQUIRE(cppfind::FileUtil::split_path("./path")[1] == "path");
    REQUIRE(cppfind::FileUtil::split_path("./path/").size() == 2);
    REQUIRE(cppfind::FileUtil::split_path("./path/")[0] == ".");
    REQUIRE(cppfind::FileUtil::split_path("./path/")[1] == "path");
}
