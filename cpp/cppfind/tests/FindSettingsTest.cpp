#include <catch2/catch.hpp>
#include "FindSettings.h"

TEST_CASE("Get default FindSettings", "[FindSettings]") {
    auto settings = cppfind::FindSettings();

    REQUIRE(!settings.archives_only());
    REQUIRE(!settings.debug());
    REQUIRE(settings.exclude_hidden());
    REQUIRE(!settings.include_archives());
    REQUIRE(!settings.list_dirs());
    REQUIRE(!settings.list_files());
    REQUIRE(settings.max_last_mod() == 0);
    REQUIRE(settings.max_size() == 0);
    REQUIRE(settings.min_last_mod() == 0);
    REQUIRE(settings.min_size() == 0);
    REQUIRE(!settings.print_usage());
    REQUIRE(!settings.print_version());
    REQUIRE(settings.sort_by() == cppfind::SortBy::FILEPATH);
    REQUIRE(!settings.sort_case_insensitive());
    REQUIRE(!settings.sort_descending());
    REQUIRE(!settings.verbose());

    REQUIRE(settings.in_archive_extensions().empty());
    REQUIRE(settings.in_archive_file_patterns().empty());
    REQUIRE(settings.in_dir_patterns().empty());
    REQUIRE(settings.in_extensions().empty());
    REQUIRE(settings.in_file_patterns().empty());
    REQUIRE(settings.in_file_types().empty());
    REQUIRE(settings.out_archive_extensions().empty());
    REQUIRE(settings.out_archive_file_patterns().empty());
    REQUIRE(settings.out_dir_patterns().empty());
    REQUIRE(settings.out_extensions().empty());
    REQUIRE(settings.out_file_patterns().empty());
    REQUIRE(settings.out_file_types().empty());
    REQUIRE(settings.paths().empty());
}

TEST_CASE("Add extensions to FindSettings", "[FindSettings]") {
    auto settings = cppfind::FindSettings();

    REQUIRE(settings.in_archive_extensions().empty());
    settings.add_in_archive_extension("zip,gz");
    REQUIRE(settings.in_archive_extensions().size() == 2);

    REQUIRE(settings.out_archive_extensions().empty());
    settings.add_out_archive_extension("rar,");
    REQUIRE(settings.out_archive_extensions().size() == 1);

    REQUIRE(settings.in_extensions().empty());
    settings.add_in_extension("cpp,h");
    REQUIRE(settings.in_extensions().size() == 2);

    REQUIRE(settings.out_extensions().empty());
    settings.add_out_extension("a,o");
    REQUIRE(settings.out_extensions().size() == 2);
}

TEST_CASE("Add patterns to FindSettings", "[FindSettings]") {
    auto settings = cppfind::FindSettings();

    REQUIRE(settings.in_archive_file_patterns().empty());
    settings.add_in_archive_file_pattern("archive");
    REQUIRE(settings.in_archive_file_patterns().size() == 1);

    REQUIRE(settings.out_archive_file_patterns().empty());
    settings.add_out_archive_file_pattern("old");
    REQUIRE(settings.out_archive_file_patterns().size() == 1);

    REQUIRE(settings.in_dir_patterns().empty());
    settings.add_in_dir_pattern("dir");
    REQUIRE(settings.in_dir_patterns().size() == 1);

    REQUIRE(settings.out_dir_patterns().empty());
    settings.add_out_dir_pattern("tmp");
    REQUIRE(settings.out_dir_patterns().size() == 1);

    REQUIRE(settings.in_file_patterns().empty());
    settings.add_in_file_pattern("file");
    REQUIRE(settings.in_file_patterns().size() == 1);

    REQUIRE(settings.out_file_patterns().empty());
    settings.add_out_file_pattern("stream");
    REQUIRE(settings.out_file_patterns().size() == 1);
}

TEST_CASE("Alter booleans in FindSettings", "[FindSettings]") {
    auto settings = cppfind::FindSettings();

    REQUIRE(!settings.archives_only());
    REQUIRE(!settings.include_archives());
    settings.archives_only(true);
    REQUIRE(settings.archives_only());
    REQUIRE(settings.include_archives());

    REQUIRE(!settings.debug());
    REQUIRE(!settings.verbose());
    settings.debug(true);
    REQUIRE(settings.debug());
    REQUIRE(settings.verbose());

    REQUIRE(settings.exclude_hidden());
    settings.exclude_hidden(false);
    REQUIRE(!settings.exclude_hidden());

    REQUIRE(!settings.list_dirs());
    settings.list_dirs(true);
    REQUIRE(settings.list_dirs());

    REQUIRE(!settings.list_files());
    settings.list_files(true);
    REQUIRE(settings.list_files());

    REQUIRE(!settings.print_usage());
    settings.print_usage(true);
    REQUIRE(settings.print_usage());

    REQUIRE(!settings.print_version());
    settings.print_version(true);
    REQUIRE(settings.print_version());

    REQUIRE(settings.recursive());
    settings.recursive(false);
    REQUIRE(!settings.recursive());
}
