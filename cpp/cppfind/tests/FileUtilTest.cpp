#include <filesystem>
#include <catch2/catch_all.hpp>
// #include <catch2/catch.hpp>

#include "FindConfig.h"
#include "FileUtil.h"

TEST_CASE("Expand paths", "[FileUtil]") {
    std::string HOME = getenv("HOME");
    std::string full_path = HOME + "/filename.txt";
    REQUIRE(cppfind::FileUtil::expand_path("filename.txt") == "filename.txt");
    REQUIRE(cppfind::FileUtil::expand_path("./filename.txt") == "./filename.txt");
    REQUIRE(cppfind::FileUtil::expand_path(full_path) == full_path);
    REQUIRE(cppfind::FileUtil::expand_path("~/filename.txt") == full_path);
    REQUIRE(cppfind::FileUtil::expand_path("~") == HOME);
    REQUIRE(cppfind::FileUtil::expand_path("~/") == HOME + "/");
}

TEST_CASE("Detect file existence", "[FileUtil]") {
    std::string xfindpath {cppfind::xfindpath()};
    REQUIRE(cppfind::FileUtil::file_exists(xfindpath + "/shared/filetypes.json"));
    REQUIRE(!cppfind::FileUtil::file_exists(xfindpath + "/nonexistant.txt"));
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
    std::string xfindpath {cppfind::xfindpath()};
    REQUIRE(cppfind::FileUtil::is_directory(xfindpath));
    REQUIRE(!cppfind::FileUtil::is_directory(xfindpath + "/shared/filetypes.json"));
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

TEST_CASE("Split current dir relative paths", "[FileUtil]") {
    std::pair<std::string, std::string> v1 = cppfind::FileUtil::split_path(".");
    REQUIRE(v1.first.empty());
    REQUIRE(v1.second == ".");
    std::pair<std::string, std::string> v2 = cppfind::FileUtil::split_path("./");
    REQUIRE(v2.first.empty());
    REQUIRE(v2.second == ".");
    std::pair<std::string, std::string> v3 = cppfind::FileUtil::split_path("./path");
    REQUIRE(v3.first == ".");
    REQUIRE(v3.second == "path");
    std::pair<std::string, std::string> v4 = cppfind::FileUtil::split_path("./path/");
    REQUIRE(v4.first == ".");
    REQUIRE(v4.second == "path");
}

TEST_CASE("Split parent dir relative paths", "[FileUtil]") {
    std::pair<std::string, std::string> v1 = cppfind::FileUtil::split_path("..");
    REQUIRE(v1.first.empty());
    REQUIRE(v1.second == "..");
    std::pair<std::string, std::string> v2 = cppfind::FileUtil::split_path("../");
    REQUIRE(v2.first.empty());
    REQUIRE(v2.second == "..");
    std::pair<std::string, std::string> v3 = cppfind::FileUtil::split_path("../path");
    REQUIRE(v3.first == "..");
    REQUIRE(v3.second == "path");
    std::pair<std::string, std::string> v4 = cppfind::FileUtil::split_path("../path/");
    REQUIRE(v4.first == "..");
    REQUIRE(v4.second == "path");
}
