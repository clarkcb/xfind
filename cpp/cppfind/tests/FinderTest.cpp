#include <catch2/catch_all.hpp>
#include "FileTypes.h"
#include "FindConfig.h"
#include "Finder.h"

cppfind::FindSettings get_settings(std::string_view path) {
    auto settings = cppfind::FindSettings();
    settings.add_path(path);
    return settings;
}

/***************************************************************************
 * filter_to_file_result tests
 **************************************************************************/
TEST_CASE("Test filter_to_file_result hidden file should be false", "[Finder]") {
    const auto settings = get_settings(".");
    const auto finder = cppfind::Finder(settings);
    REQUIRE(!finder.filter_to_file_result(".hidden.txt").has_value());
}

TEST_CASE("Test filter_to_file_result hidden file include-hidden should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.include_hidden(true);
    const auto finder = cppfind::Finder(settings);

    REQUIRE(finder.filter_to_file_result("./.hidden.txt").has_value());
}

TEST_CASE("Test filter_to_file_result archive file should be false", "[Finder]") {
    const auto settings = get_settings(".");
    const auto finder = cppfind::Finder(settings);

    REQUIRE(!finder.filter_to_file_result("archive.zip").has_value());
}

TEST_CASE("Test filter_to_file_result archive file find-archives should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.include_archives(true);
    const auto finder = cppfind::Finder(settings);

    REQUIRE(finder.filter_to_file_result("archive.zip").has_value());
}

TEST_CASE("Test filter_to_file_result archive file is_archive_file should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.include_archives(true);
    settings.add_in_archive_extension("zip");
    const auto finder = cppfind::Finder(settings);

    REQUIRE(finder.filter_to_file_result("archive.zip").has_value());
}

TEST_CASE("Test filter_to_file_result archive file !is_archive_file should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.include_archives(true);
    settings.add_out_archive_extension("zip");
    const auto finder = cppfind::Finder(settings);

    REQUIRE(!finder.filter_to_file_result("archive.zip").has_value());
}

TEST_CASE("Test filter_to_file_result archive file archives-only should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.archives_only(true);
    const auto finder = cppfind::Finder(settings);

    REQUIRE(finder.filter_to_file_result("archive.zip").has_value());
}

TEST_CASE("Test filter_to_file_result non-archive file archives-only should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.archives_only(true);
    const auto finder = cppfind::Finder(settings);

    REQUIRE(!finder.filter_to_file_result("FileUtil.cs").has_value());
}

TEST_CASE("Test filter_to_file_result no exts no patterns should be true", "[Finder]") {
    const auto settings = get_settings(".");
    const auto finder = cppfind::Finder(settings);

    REQUIRE(finder.filter_to_file_result("FileUtil.cs").has_value());
}

TEST_CASE("Test filter_to_file_result matching in-ext should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_extension("cs");
    const auto finder = cppfind::Finder(settings);

    REQUIRE(finder.filter_to_file_result("FileUtil.cs").has_value());
}

TEST_CASE("Test filter_to_file_result not matching in-ext should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_extension("cpp");
    const auto finder = cppfind::Finder(settings);

    REQUIRE(!finder.filter_to_file_result("FileUtil.cs").has_value());
}

TEST_CASE("Test filter_to_file_result matching out-ext should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_extension("cs");
    const auto finder = cppfind::Finder(settings);

    REQUIRE(!finder.filter_to_file_result("FileUtil.cs").has_value());
}

TEST_CASE("Test filter_to_file_result not matching out-ext should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_extension("cpp");
    const auto finder = cppfind::Finder(settings);

    REQUIRE(finder.filter_to_file_result("FileUtil.cs").has_value());
}

/***************************************************************************
 * is_matching_dir_path tests
 **************************************************************************/
TEST_CASE("Test is_matching_dir_path single dot should be true", "[Finder]") {
    const auto settings = get_settings(".");
    const auto finder = cppfind::Finder(settings);

    const std::filesystem::path dir_path{"."};

    REQUIRE(finder.is_matching_dir_path(dir_path));
}

TEST_CASE("Test is_matching_dir_path double dot should be true", "[Finder]") {
    const auto settings = get_settings(".");
    const auto finder = cppfind::Finder(settings);

    const std::filesystem::path dir_path{".."};

    REQUIRE(finder.is_matching_dir_path(dir_path));
}

TEST_CASE("Test is_matching_dir_path hidden dir should be false", "[Finder]") {
    const auto settings = get_settings(".");
    const auto finder = cppfind::Finder(settings);

    const std::filesystem::path dir_path{".git"};

    REQUIRE(!finder.is_matching_dir_path(dir_path));
}

TEST_CASE("Test is_matching_dir_path hidden dir include hidden should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.include_hidden(true);
    const auto finder = cppfind::Finder(settings);

    const std::filesystem::path dir_path{".git"};

    REQUIRE(finder.is_matching_dir_path(dir_path));
}

TEST_CASE("Test is_matching_dir_path no patterns should be true", "[Finder]") {
    const auto settings = get_settings(".");
    const auto finder = cppfind::Finder(settings);

    const std::filesystem::path dir_path{"/Users"};

    REQUIRE(finder.is_matching_dir_path(dir_path));
}

TEST_CASE("Test is_matching_dir_path matches in-pattern should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_dir_pattern("Find");
    const auto finder = cppfind::Finder(settings);

    const std::filesystem::path dir_path{"CsFind"};

    REQUIRE(finder.is_matching_dir_path(dir_path));
}

TEST_CASE("Test is_matching_dir_path matches out-pattern should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_dir_pattern("Find");
    const auto finder = cppfind::Finder(settings);

    const std::filesystem::path dir_path{"CsFind"};

    REQUIRE(!finder.is_matching_dir_path(dir_path));
}

TEST_CASE("Test is_matching_dir_path doesn't match in-pattern should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_dir_pattern("FindFiles");
    const auto finder = cppfind::Finder(settings);

    const std::filesystem::path dir_path{"CsFind"};

    REQUIRE(!finder.is_matching_dir_path(dir_path));
}

TEST_CASE("Test is_matching_dir doesn't match out-pattern should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_dir_pattern("FindFiles");
    const auto finder = cppfind::Finder(settings);

    const std::filesystem::path dir_path{"CsFind"};

    REQUIRE(finder.is_matching_dir_path(dir_path));
}

/***************************************************************************
 * is_matching_file_result tests
 **************************************************************************/
TEST_CASE("Test is_matching_file_result no exts no patterns should be true", "[Finder]") {
    const auto settings = get_settings(".");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "FileUtil.cs";
    constexpr auto file_type = cppfind::FileType::CODE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(finder.is_matching_file_result(file_result));
}

TEST_CASE("Test is_matching_file_result matches in-ext should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_extension("cs");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "FileUtil.cs";
    constexpr auto file_type = cppfind::FileType::CODE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(finder.is_matching_file_result(file_result));
}

TEST_CASE("Test is_matching_file_result does not match in-ext should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_extension("java");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "FileUtil.cs";
    constexpr auto file_type = cppfind::FileType::CODE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(!finder.is_matching_file_result(file_result));
}

TEST_CASE("Test is_matching_file_result matches out-ext should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_extension("cs");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "FileUtil.cs";
    constexpr auto file_type = cppfind::FileType::CODE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(!finder.is_matching_file_result(file_result));
}

TEST_CASE("Test is_matching_file_result does not match out-ext should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_extension("java");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "FileUtil.cs";
    constexpr auto file_type = cppfind::FileType::CODE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(finder.is_matching_file_result(file_result));
}

TEST_CASE("Test is_matching_file_result matches in-pattern should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_file_pattern("Finder");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "Finder.cs";
    constexpr auto file_type = cppfind::FileType::CODE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(finder.is_matching_file_result(file_result));
}

TEST_CASE("Test is_matching_file_result does not match in-pattern should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_file_pattern("Finder");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "FileUtil.cs";
    constexpr auto file_type = cppfind::FileType::CODE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(!finder.is_matching_file_result(file_result));
}

TEST_CASE("Test is_matching_file_result matches out-pattern should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_file_pattern("Finder");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "Finder.cs";
    constexpr auto file_type = cppfind::FileType::CODE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(!finder.is_matching_file_result(file_result));
}

TEST_CASE("Test is_matching_file_result does not match out-pattern should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_file_pattern("Finder");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "FileUtil.cs";
    constexpr auto file_type = cppfind::FileType::CODE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(finder.is_matching_file_result(file_result));
}

/***************************************************************************
 * is_matching_archive_file_result tests
 **************************************************************************/
TEST_CASE("Test is_matching_archive_file_result no exts no patterns should be true", "[Finder]") {
    const auto settings = get_settings(".");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "archive.zip";
    constexpr auto file_type = cppfind::FileType::ARCHIVE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(finder.is_matching_archive_file_result(file_result));
}

TEST_CASE("Test is_matching_archive_file_result matches in-ext should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_archive_extension("zip");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "archive.zip";
    constexpr auto file_type = cppfind::FileType::ARCHIVE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(finder.is_matching_archive_file_result(file_result));
}

TEST_CASE("Test is_matching_archive_file_result does not match in-ext should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_archive_extension("gz");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "archive.zip";
    constexpr auto file_type = cppfind::FileType::ARCHIVE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(!finder.is_matching_archive_file_result(file_result));
}

TEST_CASE("Test is_matching_archive_file_result matches out-ext should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_archive_extension("zip");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "archive.zip";
    constexpr auto file_type = cppfind::FileType::ARCHIVE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(!finder.is_matching_archive_file_result(file_result));
}

TEST_CASE("Test is_matching_archive_file_result does not match out-ext should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_archive_extension("gz");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "archive.zip";
    constexpr auto file_type = cppfind::FileType::ARCHIVE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(finder.is_matching_archive_file_result(file_result));
}

TEST_CASE("Test is_matching_archive_file_result matches in-pattern should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_archive_file_pattern("arch");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "archive.zip";
    constexpr auto file_type = cppfind::FileType::ARCHIVE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(finder.is_matching_archive_file_result(file_result));
}

TEST_CASE("Test is_matching_archive_file_result does not match in-pattern should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_in_archive_file_pattern("archives");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "archive.zip";
    constexpr auto file_type = cppfind::FileType::ARCHIVE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(!finder.is_matching_archive_file_result(file_result));
}

TEST_CASE("Test is_matching_archive_file_result matches out-pattern should be false", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_archive_file_pattern("arch");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "archive.zip";
    constexpr auto file_type = cppfind::FileType::ARCHIVE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(!finder.is_matching_archive_file_result(file_result));
}

TEST_CASE("Test is_matching_archive_file_result does not match out-pattern should be true", "[Finder]") {
    auto settings = get_settings(".");
    settings.add_out_archive_file_pattern("archives");
    const auto finder = cppfind::Finder(settings);

    const std::string file_path = "archive.zip";
    constexpr auto file_type = cppfind::FileType::ARCHIVE;
    constexpr uint64_t file_size = 1000;
    constexpr long mod_time = 0;
    const auto file_result = cppfind::FileResult(file_path, file_type, file_size, mod_time);

    REQUIRE(finder.is_matching_archive_file_result(file_result));
}

/***************************************************************************
 * follow_symlinks tests
 **************************************************************************/
TEST_CASE("Test follow_symlinks with default settings should exclude symlinks", "[Finder]") {
    auto settings = cppfind::FindSettings();
    const auto bin_path = cppfind::xfindpath() + "/bin";
    settings.add_path(bin_path);
    const auto finder = cppfind::Finder(settings);
    const auto file_results = finder.find();
    REQUIRE(file_results.size() < 3);
}

TEST_CASE("Test follow_symlinks with follow_symlinks should include symlinks", "[Finder]") {
    auto settings = cppfind::FindSettings();
    const auto bin_path = cppfind::xfindpath() + "/bin";
    settings.add_path(bin_path);
    settings.follow_symlinks(true);
    const auto finder = cppfind::Finder(settings);
    if (const auto file_results = finder.find();
        !file_results.empty()) {
        REQUIRE(file_results.size() > 2);
    }
}

TEST_CASE("Test follow_symlinks no follow_symlinks should exclude symlinks", "[Finder]") {
    auto settings = cppfind::FindSettings();
    const auto bin_path = cppfind::xfindpath() + "/bin";
    settings.add_path(bin_path);
    settings.follow_symlinks(false);
    const auto finder = cppfind::Finder(settings);
    const auto file_results = finder.find();
    REQUIRE(file_results.size() < 3);
}
