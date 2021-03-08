#include <catch2/catch.hpp>
#include "FindOptions.h"

TEST_CASE("Get FindSettings from minimal args", "[FindOptions]") {
    auto* options = new cppfind::FindOptions();
    char* argv[] = { const_cast<char *>("cppfind"), const_cast<char *>(".") };
    int argc = 2;
    cppfind::FindSettings* settings = options->settings_from_args(argc, argv);
    REQUIRE(!settings->archivesonly());
    REQUIRE(!settings->debug());
    REQUIRE(settings->excludehidden());
    REQUIRE(!settings->includearchives());
    REQUIRE(!settings->listdirs());
    REQUIRE(settings->listfiles());
    REQUIRE(!settings->printusage());
    REQUIRE(!settings->printversion());
    REQUIRE(!settings->verbose());

    REQUIRE(settings->in_archiveextensions()->empty());
    REQUIRE(settings->in_archivefilepatterns()->empty());
    REQUIRE(settings->in_dirpatterns()->empty());
    REQUIRE(settings->in_extensions()->empty());
    REQUIRE(settings->in_filepatterns()->empty());
    REQUIRE(settings->out_archiveextensions()->empty());
    REQUIRE(settings->out_archivefilepatterns()->empty());
    REQUIRE(settings->out_dirpatterns()->empty());
    REQUIRE(settings->out_extensions()->empty());
    REQUIRE(settings->out_filepatterns()->empty());

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
    REQUIRE(!settings->archivesonly());
    REQUIRE(!settings->debug());
    REQUIRE(settings->excludehidden());
    REQUIRE(!settings->includearchives());
    REQUIRE(!settings->listdirs());
    REQUIRE(settings->listfiles());
    REQUIRE(!settings->printusage());
    REQUIRE(!settings->printversion());
    REQUIRE(!settings->verbose());

    REQUIRE(settings->in_archiveextensions()->empty());
    REQUIRE(settings->in_archivefilepatterns()->empty());
    REQUIRE(settings->in_dirpatterns()->empty());
    REQUIRE(settings->in_filepatterns()->empty());
    REQUIRE(settings->out_archiveextensions()->empty());
    REQUIRE(settings->out_archivefilepatterns()->empty());
    REQUIRE(settings->out_dirpatterns()->empty());
    REQUIRE(settings->out_extensions()->empty());
    REQUIRE(settings->out_filepatterns()->empty());

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
    REQUIRE(settings->out_dirpatterns()->size() == 4);
    REQUIRE(settings->out_dirpatterns()->at(0)->pattern() == "build");
    REQUIRE(settings->out_dirpatterns()->at(1)->pattern() == "node_module");
    REQUIRE(settings->out_dirpatterns()->at(2)->pattern() == "tests");
    REQUIRE(settings->out_dirpatterns()->at(3)->pattern() == "typings");
    REQUIRE(settings->out_filepatterns()->size() == 2);
    REQUIRE(settings->out_filepatterns()->at(0)->pattern() == "gulpfile");
    REQUIRE(settings->out_filepatterns()->at(1)->pattern() == "\\.min\\.");
    REQUIRE(settings->debug() == true);
    REQUIRE(settings->excludehidden() == true);
}
