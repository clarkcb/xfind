#include <catch2/catch.hpp>
#include "FindOptions.h"
#include "RegexPattern.h"

TEST_CASE("Get FindSettings from minimal args", "[FindOptions]") {
    auto* options = new cppfind::FindOptions();
    char* argv[] = { const_cast<char *>("cppfind"), const_cast<char *>(".") };
    int argc = 2;
    cppfind::FindSettings* settings = options->settings_from_args(argc, argv);
    REQUIRE(!settings->archives_only());
    REQUIRE(!settings->debug());
    REQUIRE(!settings->include_archives());
    REQUIRE(!settings->include_hidden());
    REQUIRE(!settings->print_dirs());
    REQUIRE(settings->print_files());
    REQUIRE(!settings->print_usage());
    REQUIRE(!settings->print_version());
    REQUIRE(!settings->verbose());

    REQUIRE(settings->in_archive_extensions().empty());
    REQUIRE(settings->in_archive_file_patterns().empty());
    REQUIRE(settings->in_dir_patterns().empty());
    REQUIRE(settings->in_extensions().empty());
    REQUIRE(settings->in_file_patterns().empty());
    REQUIRE(settings->out_archive_extensions().empty());
    REQUIRE(settings->out_archive_file_patterns().empty());
    REQUIRE(settings->out_dir_patterns().empty());
    REQUIRE(settings->out_extensions().empty());
    REQUIRE(settings->out_file_patterns().empty());

    std::set<std::string> paths = settings->paths();
    REQUIRE(paths.size() == 1);
    REQUIRE(paths.contains("."));
}

TEST_CASE("Get FindSettings from valid args", "[FindOptions]") {
    auto* options = new cppfind::FindOptions();
    char* argv[] = { const_cast<char *>("cppfind"), const_cast<char *>("-x"),
                     const_cast<char *>("java,scala"), const_cast<char *>(".") };
    int argc = 4;
    cppfind::FindSettings* settings = options->settings_from_args(argc, argv);
    REQUIRE(!settings->archives_only());
    REQUIRE(!settings->debug());
    REQUIRE(!settings->include_archives());
    REQUIRE(!settings->include_hidden());
    REQUIRE(!settings->print_dirs());
    REQUIRE(settings->print_files());
    REQUIRE(!settings->print_usage());
    REQUIRE(!settings->print_version());
    REQUIRE(!settings->verbose());

    REQUIRE(settings->in_archive_extensions().empty());
    REQUIRE(settings->in_archive_file_patterns().empty());
    REQUIRE(settings->in_dir_patterns().empty());
    REQUIRE(settings->in_file_patterns().empty());
    REQUIRE(settings->out_archive_extensions().empty());
    REQUIRE(settings->out_archive_file_patterns().empty());
    REQUIRE(settings->out_dir_patterns().empty());
    REQUIRE(settings->out_extensions().empty());
    REQUIRE(settings->out_file_patterns().empty());

    std::set<std::string> in_exts = settings->in_extensions();
    REQUIRE(in_exts.size() == 2);
    REQUIRE(in_exts.contains("java"));
    REQUIRE(in_exts.contains("scala"));

    std::set<std::string> paths = settings->paths();
    REQUIRE(paths.size() == 1);
    REQUIRE(paths.contains("."));
}

bool set_has_pattern(std::set<cppfind::RegexPattern, cppfind::RegexPatternCmp> set, std::string pattern) {
    std::smatch pmatch;
    for (auto& r : set) {
        if (r.pattern() == pattern) {
            return true;
        }
    }
    return false;
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
    options->settings_from_json(json, *settings);

    REQUIRE(settings->paths().size() == 1);
    REQUIRE(settings->paths().contains("~/src/xfind/"));
    REQUIRE(settings->in_extensions().size() == 2);
    REQUIRE(settings->in_extensions().contains("js"));
    REQUIRE(settings->in_extensions().contains("ts"));
    REQUIRE(settings->out_dir_patterns().size() == 4);
    REQUIRE(set_has_pattern(settings->out_dir_patterns(), "build"));
    REQUIRE(set_has_pattern(settings->out_dir_patterns(), "node_module"));
    REQUIRE(set_has_pattern(settings->out_dir_patterns(), "tests"));
    REQUIRE(set_has_pattern(settings->out_dir_patterns(), "typings"));
    REQUIRE(settings->out_file_patterns().size() == 2);
    REQUIRE(set_has_pattern(settings->out_file_patterns(), "gulpfile"));
    REQUIRE(set_has_pattern(settings->out_file_patterns(), "\\.min\\."));
    REQUIRE(settings->debug() == true);
    REQUIRE(settings->include_hidden() == false);
}
