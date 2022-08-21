#include <config.h>
#include <catch2/catch.hpp>
#include "FileUtil.h"

TEST_CASE("Expand paths", "[FileUtil]") {
    std::string HOME = getenv("HOME");
    std::string fullpath = HOME + "/filename.txt";
    REQUIRE(cppfind::FileUtil::expand_path("filename.txt") == "filename.txt");
    REQUIRE(cppfind::FileUtil::expand_path("./filename.txt") == "./filename.txt");
    REQUIRE(cppfind::FileUtil::expand_path(fullpath) == fullpath);
    REQUIRE(cppfind::FileUtil::expand_path("~/filename.txt") == fullpath);
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
    std::vector<std::string> v1 = cppfind::FileUtil::split_path(".");
    REQUIRE(v1.size() == 1);
    REQUIRE(v1[0] == ".");
    std::vector<std::string> v2 = cppfind::FileUtil::split_path("./");
    REQUIRE(v2.size() == 1);
    REQUIRE(v2[0] == ".");
    std::vector<std::string> v3 = cppfind::FileUtil::split_path("./path");
    REQUIRE(v3.size() == 2);
    REQUIRE(v3[0] == ".");
    REQUIRE(v3[1] == "path");
    std::vector<std::string> v4 = cppfind::FileUtil::split_path("./path/");
    REQUIRE(v4.size() == 2);
    REQUIRE(v4[0] == ".");
    REQUIRE(v4[1] == "path");
}

TEST_CASE("Split parent dir relative paths", "[FileUtil]") {
    std::vector<std::string> v1 = cppfind::FileUtil::split_path("..");
    REQUIRE(v1.size() == 1);
    REQUIRE(v1[0] == "..");
    std::vector<std::string> v2 = cppfind::FileUtil::split_path("../");
    REQUIRE(v2.size() == 1);
    REQUIRE(v2[0] == "..");
    std::vector<std::string> v3 = cppfind::FileUtil::split_path("../path");
    REQUIRE(v3.size() == 2);
    REQUIRE(v3[0] == "..");
    REQUIRE(v3[1] == "path");
    std::vector<std::string> v4 = cppfind::FileUtil::split_path("../path/");
    REQUIRE(v4.size() == 2);
    REQUIRE(v4[0] == "..");
    REQUIRE(v4[1] == "path");
}
