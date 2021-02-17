#include <catch2/catch.hpp>
#include "config.h"
#include "Finder.h"

/***************************************************************************
 * filter_file tests
 **************************************************************************/
TEST_CASE("Test filter_file hidden file should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->filter_file(".hidden.txt"));
}

TEST_CASE("Test filter_file hidden file include-hidden should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->excludehidden(false);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_file(".hidden.txt"));
}

TEST_CASE("Test filter_file archive file should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->filter_file("archive.zip"));
}

TEST_CASE("Test filter_file archive file find-archives should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->findarchives(true);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_file("archive.zip"));
}

TEST_CASE("Test filter_file archive file is_archive_file should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->findarchives(true);
    settings->add_in_archiveextension("zip");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_file("archive.zip"));
}

TEST_CASE("Test filter_file archive file !is_archive_file should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->findarchives(true);
    settings->add_out_archiveextension("zip");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->filter_file("archive.zip"));
}

TEST_CASE("Test filter_file archive file archives-only should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->archivesonly(true);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_file("archive.zip"));
}

TEST_CASE("Test filter_file non-archive file archives-only should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->archivesonly(true);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->filter_file("FileUtil.cs"));
}

TEST_CASE("Test filter_file no exts no patterns should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_file("FileUtil.cs"));
}

TEST_CASE("Test filter_file matching in-ext should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_in_extension("cs");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_file("FileUtil.cs"));
}

TEST_CASE("Test filter_file not matching in-ext should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_in_extension("cpp");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->filter_file("FileUtil.cs"));
}

TEST_CASE("Test filter_file matching out-ext should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_out_extension("cs");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->filter_file("FileUtil.cs"));
}

TEST_CASE("Test filter_file not matching out-ext should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_out_extension("cpp");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->filter_file("FileUtil.cs"));
}

/***************************************************************************
 * is_find_dir tests
 **************************************************************************/
TEST_CASE("Test is_find_dir single dot should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_find_dir("."));
}

TEST_CASE("Test is_find_dir double dot should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_find_dir(".."));
}

TEST_CASE("Test is_find_dir hidden dir should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_find_dir(".git"));
}

TEST_CASE("Test is_find_dir hidden dir include hidden should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->excludehidden(false);
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_find_dir(".git"));
}

TEST_CASE("Test is_find_dir no patterns should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_find_dir("Users"));
}

TEST_CASE("Test is_find_dir matches in-pattern should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_in_dirpattern("Find");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_find_dir("CsFind"));
}

TEST_CASE("Test is_find_dir matches out-pattern should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_out_dirpattern("Find");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_find_dir("CsFind"));
}

TEST_CASE("Test is_find_dir doesn't match in-pattern should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_in_dirpattern("FindFiles");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_find_dir("CsFind"));
}

TEST_CASE("Test is_find_dir doesn't match out-pattern should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_out_dirpattern("FindFiles");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_find_dir("CsFind"));
}

/***************************************************************************
 * is_find_file tests
 **************************************************************************/
TEST_CASE("Test is_find_file no exts no patterns should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_find_file("FileUtil.cs"));
}

TEST_CASE("Test is_find_file matches in-ext should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_in_extension("cs");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_find_file("FileUtil.cs"));
}

TEST_CASE("Test is_find_file does not match in-ext should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_in_extension("java");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_find_file("FileUtil.cs"));
}

TEST_CASE("Test is_find_file matches out-ext should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_out_extension("cs");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_find_file("FileUtil.cs"));
}

TEST_CASE("Test is_find_file does not match out-ext should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_out_extension("java");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_find_file("FileUtil.cs"));
}

TEST_CASE("Test is_find_file matches in-pattern should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_in_filepattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_find_file("Finder.cs"));
}

TEST_CASE("Test is_find_file does not match in-pattern should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_in_filepattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_find_file("FileUtil.cs"));
}

TEST_CASE("Test is_find_file matches out-pattern should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_out_filepattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_find_file("Finder.cs"));
}

TEST_CASE("Test is_find_file does not match out-pattern should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_out_filepattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_find_file("FileUtil.cs"));
}

/***************************************************************************
 * is_archive_find_file tests
 **************************************************************************/
TEST_CASE("Test is_archive_find_file no exts no patterns should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_archive_find_file("archive.zip"));
}

TEST_CASE("Test is_archive_find_file matches in-ext should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_in_archiveextension("zip");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_archive_find_file("archive.zip"));
}

TEST_CASE("Test is_archive_find_file does not match in-ext should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_in_archiveextension("gz");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_archive_find_file("archive.zip"));
}

TEST_CASE("Test is_archive_find_file matches out-ext should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_out_archiveextension("zip");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_archive_find_file("archive.zip"));
}

TEST_CASE("Test is_archive_find_file does not match out-ext should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_out_archiveextension("gz");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_archive_find_file("archive.zip"));
}

TEST_CASE("Test is_archive_find_file matches in-pattern should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_in_archivefilepattern("arch");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_archive_find_file("archive.zip"));
}

TEST_CASE("Test is_archive_find_file does not match in-pattern should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_in_archivefilepattern("archives");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_archive_find_file("archive.zip"));
}

TEST_CASE("Test is_archive_find_file matches out-pattern should be false", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_out_archivefilepattern("arch");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(!finder->is_archive_find_file("archive.zip"));
}

TEST_CASE("Test is_archive_find_file does not match out-pattern should be true", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = ".";
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->add_out_archivefilepattern("archives");
    auto *finder = new cppfind::Finder(settings);

    REQUIRE(finder->is_archive_find_file("archive.zip"));
}

/***************************************************************************
 * find tests
 **************************************************************************/
TEST_CASE("Test find with test file startpath", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = std::string(XFINDPATH);
    startpath.append("/shared/testFiles/testFile2.txt");
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    auto *finder = new cppfind::Finder(settings);

    auto results = finder->find();

    REQUIRE(results.size() == 2);
    REQUIRE(results[0]->linenum() == 29);
    REQUIRE(results[0]->match_start_idx() == 3);
    REQUIRE(results[0]->match_end_idx() == 11);
    REQUIRE(results[1]->linenum() == 35);
    REQUIRE(results[1]->match_start_idx() == 24);
    REQUIRE(results[1]->match_end_idx() == 32);
}

TEST_CASE("Test find with in lines before matching", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = std::string(XFINDPATH);
    startpath.append("/shared/testFiles/testFile2.txt");
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->linesbefore(2);
    settings->add_in_linesbeforepattern("FileUtil");
    auto *finder = new cppfind::Finder(settings);

    auto results = finder->find();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0]->linenum() == 29);
    REQUIRE(results[0]->match_start_idx() == 3);
    REQUIRE(results[0]->match_end_idx() == 11);
}

TEST_CASE("Test find with out lines before matching", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = std::string(XFINDPATH);
    startpath.append("/shared/testFiles/testFile2.txt");
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->linesbefore(2);
    settings->add_out_linesbeforepattern("FileUtil");
    auto *finder = new cppfind::Finder(settings);

    auto results = finder->find();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0]->linenum() == 35);
    REQUIRE(results[0]->match_start_idx() == 24);
    REQUIRE(results[0]->match_end_idx() == 32);
}

TEST_CASE("Test find with in lines after matching", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = std::string(XFINDPATH);
    startpath.append("/shared/testFiles/testFile2.txt");
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->linesafter(2);
    settings->add_in_linesafterpattern("Settings");
    auto *finder = new cppfind::Finder(settings);

    auto results = finder->find();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0]->linenum() == 29);
    REQUIRE(results[0]->match_start_idx() == 3);
    REQUIRE(results[0]->match_end_idx() == 11);
}

TEST_CASE("Test find with out lines after matching", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = std::string(XFINDPATH);
    startpath.append("/shared/testFiles/testFile2.txt");
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->linesafter(2);
    settings->add_out_linesafterpattern("Settings");
    auto *finder = new cppfind::Finder(settings);

    auto results = finder->find();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0]->linenum() == 35);
    REQUIRE(results[0]->match_start_idx() == 24);
    REQUIRE(results[0]->match_end_idx() == 32);
}

TEST_CASE("Test multiline find with test file startpath", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = std::string(XFINDPATH);
    startpath.append("/shared/testFiles/testFile2.txt");
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->multilineoption-REMOVE(true);
    auto *finder = new cppfind::Finder(settings);

    auto results = finder->find();

    REQUIRE(results.size() == 2);
    REQUIRE(results[0]->linenum() == 29);
    REQUIRE(results[0]->match_start_idx() == 3);
    REQUIRE(results[0]->match_end_idx() == 11);
    REQUIRE(results[1]->linenum() == 35);
    REQUIRE(results[1]->match_start_idx() == 24);
    REQUIRE(results[1]->match_end_idx() == 32);
}

TEST_CASE("Test multiline find with in lines before matching", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = std::string(XFINDPATH);
    startpath.append("/shared/testFiles/testFile2.txt");
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->multilineoption-REMOVE(true);
    settings->linesbefore(2);
    settings->add_in_linesbeforepattern("FileUtil");
    auto *finder = new cppfind::Finder(settings);

    auto results = finder->find();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0]->linenum() == 29);
    REQUIRE(results[0]->match_start_idx() == 3);
    REQUIRE(results[0]->match_end_idx() == 11);
}

TEST_CASE("Test multiline find with out lines before matching", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = std::string(XFINDPATH);
    startpath.append("/shared/testFiles/testFile2.txt");
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->multilineoption-REMOVE(true);
    settings->linesbefore(2);
    settings->add_out_linesbeforepattern("FileUtil");
    auto *finder = new cppfind::Finder(settings);

    auto results = finder->find();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0]->linenum() == 35);
    REQUIRE(results[0]->match_start_idx() == 24);
    REQUIRE(results[0]->match_end_idx() == 32);
}

TEST_CASE("Test multiline find with in lines after matching", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = std::string(XFINDPATH);
    startpath.append("/shared/testFiles/testFile2.txt");
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->multilineoption-REMOVE(true);
    settings->linesafter(2);
    settings->add_in_linesafterpattern("Settings");
    auto *finder = new cppfind::Finder(settings);

    auto results = finder->find();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0]->linenum() == 29);
    REQUIRE(results[0]->match_start_idx() == 3);
    REQUIRE(results[0]->match_end_idx() == 11);
}

TEST_CASE("Test multiline find with out lines after matching", "[Finder]") {
    auto *settings = new cppfind::FindSettings();
    std::string startpath = std::string(XFINDPATH);
    startpath.append("/shared/testFiles/testFile2.txt");
    settings->startpath(startpath);
    settings->add_findpattern("Finder");
    settings->multilineoption-REMOVE(true);
    settings->linesafter(2);
    settings->add_out_linesafterpattern("Settings");
    auto *finder = new cppfind::Finder(settings);

    auto results = finder->find();

    REQUIRE(results.size() == 1);
    REQUIRE(results[0]->linenum() == 35);
    REQUIRE(results[0]->match_start_idx() == 24);
    REQUIRE(results[0]->match_end_idx() == 32);
}
