#include <catch2/catch.hpp>
#include "config.h"
#include "FileTypes.h"
#include "Finder.h"

cppfind::FindSettings get_settings(std::string path) {
    auto settings = cppfind::FindSettings();
    settings.add_path(path);
    return settings;
}

/***************************************************************************
 * filter_to_file_result tests
 **************************************************************************/
TEST_CASE("Test filter_to_file_result hidden file should be false", "[Finder]") {
    auto settings = get_settings(".");
    auto *finder = new cppfind::Finder(settings);
    REQUIRE(!finder->filter_to_file_result(".hidden.txt").has_value());
}

TEST_CASE("Test filter_to_file_result hidden file include-hidden should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.include_hidden(true);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_to_file_result("./.hidden.txt").has_value());
}

TEST_CASE("Test filter_to_file_result archive file should be false", "[Finder]") {
    auto settings = get_settings(".");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->filter_to_file_result("archive.zip").has_value());
}

TEST_CASE("Test filter_to_file_result archive file find-archives should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.include_archives(true);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_to_file_result("archive.zip").has_value());
}

TEST_CASE("Test filter_to_file_result archive file is_archive_file should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.include_archives(true);
    settings.add_in_archive_extension("zip");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_to_file_result("archive.zip").has_value());
}

TEST_CASE("Test filter_to_file_result archive file !is_archive_file should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.include_archives(true);
    settings.add_out_archive_extension("zip");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->filter_to_file_result("archive.zip").has_value());
}

TEST_CASE("Test filter_to_file_result archive file archives-only should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.archives_only(true);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_to_file_result("archive.zip").has_value());
}

TEST_CASE("Test filter_to_file_result non-archive file archives-only should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.archives_only(true);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->filter_to_file_result("FileUtil.cs").has_value());
}

TEST_CASE("Test filter_to_file_result no exts no patterns should be true", "[Finder]") {
    auto settings = get_settings(".");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_to_file_result("FileUtil.cs").has_value());
}

TEST_CASE("Test filter_to_file_result matching in-ext should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_extension("cs");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_to_file_result("FileUtil.cs").has_value());
}

TEST_CASE("Test filter_to_file_result not matching in-ext should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_extension("cpp");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->filter_to_file_result("FileUtil.cs").has_value());
}

TEST_CASE("Test filter_to_file_result matching out-ext should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_extension("cs");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->filter_to_file_result("FileUtil.cs").has_value());
}

TEST_CASE("Test filter_to_file_result not matching out-ext should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_extension("cpp");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_to_file_result("FileUtil.cs").has_value());
}

/***************************************************************************
 * is_matching_dir tests
 **************************************************************************/
TEST_CASE("Test is_matching_dir single dot should be true", "[Finder]") {
    auto settings = get_settings(".");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_dir("."));
}

TEST_CASE("Test is_matching_dir double dot should be true", "[Finder]") {
    auto settings = get_settings(".");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_dir(".."));
}

TEST_CASE("Test is_matching_dir hidden dir should be false", "[Finder]") {
    auto settings = get_settings(".");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_dir(".git"));
}

TEST_CASE("Test is_matching_dir hidden dir include hidden should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.include_hidden(true);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_dir(".git"));
}

TEST_CASE("Test is_matching_dir no patterns should be true", "[Finder]") {
    auto settings = get_settings(".");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_dir("Users"));
}

TEST_CASE("Test is_matching_dir matches in-pattern should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_dir_pattern("Find");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_dir("CsFind"));
}

TEST_CASE("Test is_matching_dir matches out-pattern should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_dir_pattern("Find");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_dir("CsFind"));
}

TEST_CASE("Test is_matching_dir doesn't match in-pattern should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_dir_pattern("FindFiles");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_dir("CsFind"));
}

TEST_CASE("Test is_matching_dir doesn't match out-pattern should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_dir_pattern("FindFiles");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_dir("CsFind"));
}

/***************************************************************************
 * is_matching_file tests
 **************************************************************************/
TEST_CASE("Test is_matching_file no exts no patterns should be true", "[Finder]") {
    auto settings = get_settings(".");
    auto *finder = new cppfind::Finder(settings);
    struct stat fpstat;
    fpstat.st_size = 1000;

    REQUIRE(finder->is_matching_file("FileUtil.cs", cppfind::FileType::CODE, &fpstat));
}

TEST_CASE("Test is_matching_file matches in-ext should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_extension("cs");
    auto *finder = new cppfind::Finder(settings);
    struct stat fpstat;
    fpstat.st_size = 1000;

    REQUIRE(finder->is_matching_file("FileUtil.cs", cppfind::FileType::CODE, &fpstat));
}

TEST_CASE("Test is_matching_file does not match in-ext should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_extension("java");
    auto *finder = new cppfind::Finder(settings);
    struct stat fpstat;
    fpstat.st_size = 1000;

    REQUIRE(!finder->is_matching_file("FileUtil.cs", cppfind::FileType::CODE, &fpstat));
}

TEST_CASE("Test is_matching_file matches out-ext should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_extension("cs");
    auto *finder = new cppfind::Finder(settings);
    struct stat fpstat;
    fpstat.st_size = 1000;

    REQUIRE(!finder->is_matching_file("FileUtil.cs", cppfind::FileType::CODE, &fpstat));
}

TEST_CASE("Test is_matching_file does not match out-ext should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_extension("java");
    auto *finder = new cppfind::Finder(settings);
    struct stat fpstat;
    fpstat.st_size = 1000;

    REQUIRE(finder->is_matching_file("FileUtil.cs", cppfind::FileType::CODE, &fpstat));
}

TEST_CASE("Test is_matching_file matches in-pattern should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_file_pattern("Finder");
    auto *finder = new cppfind::Finder(settings);
    struct stat fpstat;
    fpstat.st_size = 1000;

    REQUIRE(finder->is_matching_file("Finder.cs", cppfind::FileType::CODE, &fpstat));
}

TEST_CASE("Test is_matching_file does not match in-pattern should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_file_pattern("Finder");
    auto *finder = new cppfind::Finder(settings);
    struct stat fpstat;
    fpstat.st_size = 1000;

    REQUIRE(!finder->is_matching_file("FileUtil.cs", cppfind::FileType::CODE, &fpstat));
}

TEST_CASE("Test is_matching_file matches out-pattern should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_file_pattern("Finder");
    auto *finder = new cppfind::Finder(settings);
    struct stat fpstat;
    fpstat.st_size = 1000;

    REQUIRE(!finder->is_matching_file("Finder.cs", cppfind::FileType::CODE, &fpstat));
}

TEST_CASE("Test is_matching_file does not match out-pattern should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_file_pattern("Finder");
    auto *finder = new cppfind::Finder(settings);
    struct stat fpstat;
    fpstat.st_size = 1000;

    REQUIRE(finder->is_matching_file("FileUtil.cs", cppfind::FileType::CODE, &fpstat));
}

/***************************************************************************
 * is_matching_archive_file tests
 **************************************************************************/
TEST_CASE("Test is_matching_archive_file no exts no patterns should be true", "[Finder]") {
    auto settings = get_settings(".");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_archive_file("archive.zip"));
}

TEST_CASE("Test is_matching_archive_file matches in-ext should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_archive_extension("zip");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_archive_file("archive.zip"));
}

TEST_CASE("Test is_matching_archive_file does not match in-ext should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_archive_extension("gz");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_archive_file("archive.zip"));
}

TEST_CASE("Test is_matching_archive_file matches out-ext should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_archive_extension("zip");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_archive_file("archive.zip"));
}

TEST_CASE("Test is_matching_archive_file does not match out-ext should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_archive_extension("gz");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_archive_file("archive.zip"));
}

TEST_CASE("Test is_matching_archive_file matches in-pattern should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_archive_file_pattern("arch");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_archive_file("archive.zip"));
}

TEST_CASE("Test is_matching_archive_file does not match in-pattern should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_archive_file_pattern("archives");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_archive_file("archive.zip"));
}

TEST_CASE("Test is_matching_archive_file matches out-pattern should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_archive_file_pattern("arch");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_archive_file("archive.zip"));
}

TEST_CASE("Test is_matching_archive_file does not match out-pattern should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_archive_file_pattern("archives");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_archive_file("archive.zip"));
}
