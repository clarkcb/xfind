#include <catch2/catch.hpp>
#include "config.h"
#include "FileTypes.h"
#include "Finder.h"

/***************************************************************************
 * filter_file tests
 **************************************************************************/
TEST_CASE("Test filter_file hidden file should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->filter_file(".hidden.txt"));
}

TEST_CASE("Test filter_file hidden file include-hidden should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->excludehidden(false);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_file(".hidden.txt"));
}

TEST_CASE("Test filter_file archive file should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->filter_file("archive.zip"));
}

TEST_CASE("Test filter_file archive file find-archives should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->includearchives(true);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_file("archive.zip"));
}

TEST_CASE("Test filter_file archive file is_archive_file should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->includearchives(true);
    settings->add_in_archiveextension("zip");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_file("archive.zip"));
}

TEST_CASE("Test filter_file archive file !is_archive_file should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->includearchives(true);
    settings->add_out_archiveextension("zip");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->filter_file("archive.zip"));
}

TEST_CASE("Test filter_file archive file archives-only should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->archivesonly(true);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_file("archive.zip"));
}

TEST_CASE("Test filter_file non-archive file archives-only should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->archivesonly(true);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->filter_file("FileUtil.cs"));
}

TEST_CASE("Test filter_file no exts no patterns should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_file("FileUtil.cs"));
}

TEST_CASE("Test filter_file matching in-ext should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_in_extension("cs");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_file("FileUtil.cs"));
}

TEST_CASE("Test filter_file not matching in-ext should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_in_extension("cpp");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->filter_file("FileUtil.cs"));
}

TEST_CASE("Test filter_file matching out-ext should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_out_extension("cs");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->filter_file("FileUtil.cs"));
}

TEST_CASE("Test filter_file not matching out-ext should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_out_extension("cpp");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_file("FileUtil.cs"));
}

/***************************************************************************
 * is_matching_dir tests
 **************************************************************************/
TEST_CASE("Test is_matching_dir single dot should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_dir("."));
}

TEST_CASE("Test is_matching_dir double dot should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_dir(".."));
}

TEST_CASE("Test is_matching_dir hidden dir should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_dir(".git"));
}

TEST_CASE("Test is_matching_dir hidden dir include hidden should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->excludehidden(false);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_dir(".git"));
}

TEST_CASE("Test is_matching_dir no patterns should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_dir("Users"));
}

TEST_CASE("Test is_matching_dir matches in-pattern should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_in_dirpattern("Find");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_dir("CsFind"));
}

TEST_CASE("Test is_matching_dir matches out-pattern should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_out_dirpattern("Find");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_dir("CsFind"));
}

TEST_CASE("Test is_matching_dir doesn't match in-pattern should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_in_dirpattern("FindFiles");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_dir("CsFind"));
}

TEST_CASE("Test is_matching_dir doesn't match out-pattern should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_out_dirpattern("FindFiles");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_dir("CsFind"));
}

/***************************************************************************
 * is_matching_file tests
 **************************************************************************/
TEST_CASE("Test is_matching_file no exts no patterns should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_file("FileUtil.cs", cppfind::FileType::CODE));
}

TEST_CASE("Test is_matching_file matches in-ext should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_in_extension("cs");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_file("FileUtil.cs", cppfind::FileType::CODE));
}

TEST_CASE("Test is_matching_file does not match in-ext should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_in_extension("java");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_file("FileUtil.cs", cppfind::FileType::CODE));
}

TEST_CASE("Test is_matching_file matches out-ext should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_out_extension("cs");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_file("FileUtil.cs", cppfind::FileType::CODE));
}

TEST_CASE("Test is_matching_file does not match out-ext should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_out_extension("java");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_file("FileUtil.cs", cppfind::FileType::CODE));
}

TEST_CASE("Test is_matching_file matches in-pattern should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_in_filepattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_file("Finder.cs", cppfind::FileType::CODE));
}

TEST_CASE("Test is_matching_file does not match in-pattern should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_in_filepattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_file("FileUtil.cs", cppfind::FileType::CODE));
}

TEST_CASE("Test is_matching_file matches out-pattern should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_out_filepattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_file("Finder.cs", cppfind::FileType::CODE));
}

TEST_CASE("Test is_matching_file does not match out-pattern should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_out_filepattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_file("FileUtil.cs", cppfind::FileType::CODE));
}

/***************************************************************************
 * is_matching_archive_file tests
 **************************************************************************/
TEST_CASE("Test is_matching_archive_file no exts no patterns should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_archive_file("archive.zip"));
}

TEST_CASE("Test is_matching_archive_file matches in-ext should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_in_archiveextension("zip");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_archive_file("archive.zip"));
}

TEST_CASE("Test is_matching_archive_file does not match in-ext should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_in_archiveextension("gz");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_archive_file("archive.zip"));
}

TEST_CASE("Test is_matching_archive_file matches out-ext should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_out_archiveextension("zip");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_archive_file("archive.zip"));
}

TEST_CASE("Test is_matching_archive_file does not match out-ext should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_out_archiveextension("gz");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_archive_file("archive.zip"));
}

TEST_CASE("Test is_matching_archive_file matches in-pattern should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_in_archivefilepattern("arch");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_archive_file("archive.zip"));
}

TEST_CASE("Test is_matching_archive_file does not match in-pattern should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_in_archivefilepattern("archives");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_archive_file("archive.zip"));
}

TEST_CASE("Test is_matching_archive_file matches out-pattern should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_out_archivefilepattern("arch");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_matching_archive_file("archive.zip"));
}

TEST_CASE("Test is_matching_archive_file does not match out-pattern should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->add_path(startpath);
    settings->add_out_archivefilepattern("archives");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_matching_archive_file("archive.zip"));
}
