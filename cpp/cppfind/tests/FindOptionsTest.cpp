#include <catch2/catch.hpp>
#include "FindOptions.h"

TEST_CASE("Get FindSettings from minimal args", "[FindOptions]") {
    auto* options = new cppfind::FindOptions();
    char* argv[] = { const_cast<char *>("cppfind"), const_cast<char *>(".") };
    int argc = 2;
    cppfind::FindSettings* settings = options->settings_from_args(argc, argv);
    REQUIRE(!settings->archives_only());
    REQUIRE(!settings->debug());
    REQUIRE(settings->exclude_hidden());
    REQUIRE(!settings->include_archives());
    REQUIRE(!settings->list_dirs());
    REQUIRE(settings->list_files());
    REQUIRE(!settings->print_usage());
    REQUIRE(!settings->print_version());
    REQUIRE(!settings->verbose());

    REQUIRE(settings->in_archive_extensions()->empty());
    REQUIRE(settings->in_archive_file_patterns()->empty());
    REQUIRE(settings->in_dir_patterns()->empty());
    REQUIRE(settings->in_extensions()->empty());
    REQUIRE(settings->in_file_patterns()->empty());
    REQUIRE(settings->out_archive_extensions()->empty());
    REQUIRE(settings->out_archive_file_patterns()->empty());
    REQUIRE(settings->out_dir_patterns()->empty());
    REQUIRE(settings->out_extensions()->empty());
    REQUIRE(settings->out_file_patterns()->empty());

    std::vector<std::string>* paths = settings->paths();
    REQUIRE(paths->size() == 1);
    REQUIRE(paths->at(0) == ".");
}

TEST_CASE("Get FindSettings from valid args", "[FindOptions]") {
    auto* options = new cppfind::FindOptions();
    char* argv[] = { const_cast<char *>("cppfind"), const_cast<char *>("-x"),
                     const_cast<char *>("java,scala"), const_cast<char *>(".") };
    int argc = 4;
    cppfind::FindSettings* settings = options->settings_from_args(argc, argv);
    REQUIRE(!settings->archives_only());
    REQUIRE(!settings->debug());
    REQUIRE(settings->exclude_hidden());
    REQUIRE(!settings->include_archives());
    REQUIRE(!settings->list_dirs());
    REQUIRE(settings->list_files());
    REQUIRE(!settings->print_usage());
    REQUIRE(!settings->print_version());
    REQUIRE(!settings->verbose());

    REQUIRE(settings->in_archive_extensions()->empty());
    REQUIRE(settings->in_archive_file_patterns()->empty());
    REQUIRE(settings->in_dir_patterns()->empty());
    REQUIRE(settings->in_file_patterns()->empty());
    REQUIRE(settings->out_archive_extensions()->empty());
    REQUIRE(settings->out_archive_file_patterns()->empty());
    REQUIRE(settings->out_dir_patterns()->empty());
    REQUIRE(settings->out_extensions()->empty());
    REQUIRE(settings->out_file_patterns()->empty());

    std::vector<std::string>* in_exts = settings->in_extensions();
    REQUIRE(in_exts->size() == 2);
    REQUIRE(in_exts->at(0) == "java");
    REQUIRE(in_exts->at(1) == "scala");

    std::vector<std::string>* paths = settings->paths();
    REQUIRE(paths->size() == 1);
    REQUIRE(paths->at(0) == ".");
}

TEST_CASE("Get FindSettings from JSON", "[FindOptions]") {
    std::string json = R"(
{
    "path": "~/src/xfind/",
    "in-ext": ["js","ts"],
    "out-dirpattern": ["build", "node_module", "tests", "typings"],
    "out-filepattern": ["gulpfile", "\\.min\\."],
    "debug": true,
    "includehidden": false
}
)";

    auto *options = new cppfind::FindOptions();
    auto *settings = new cppfind::FindSettings();
    options->settings_from_json(json, settings);

    REQUIRE(settings->paths()->size() == 1);
    REQUIRE(settings->paths()->at(0) == "~/src/xfind/");
    REQUIRE(settings->in_extensions()->size() == 2);
    REQUIRE(settings->in_extensions()->at(0) == "js");
    REQUIRE(settings->in_extensions()->at(1) == "ts");
    REQUIRE(settings->out_dir_patterns()->size() == 4);
    REQUIRE(settings->out_dir_patterns()->at(0)->pattern() == "build");
    REQUIRE(settings->out_dir_patterns()->at(1)->pattern() == "node_module");
    REQUIRE(settings->out_dir_patterns()->at(2)->pattern() == "tests");
    REQUIRE(settings->out_dir_patterns()->at(3)->pattern() == "typings");
    REQUIRE(settings->out_file_patterns()->size() == 2);
    REQUIRE(settings->out_file_patterns()->at(0)->pattern() == "gulpfile");
    REQUIRE(settings->out_file_patterns()->at(1)->pattern() == "\\.min\\.");
    REQUIRE(settings->debug() == true);
    REQUIRE(settings->exclude_hidden() == true);
}
