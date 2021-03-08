#include <catch2/catch.hpp>
#include "FindSettings.h"

TEST_CASE("Get default FindSettings", "[FindSettings]") {
    auto* settings = new cppfind::FindSettings();

    REQUIRE(!settings->archivesonly());
    REQUIRE(!settings->debug());
    REQUIRE(settings->excludehidden());
    REQUIRE(!settings->listdirs());
    REQUIRE(!settings->listfiles());
    REQUIRE(!settings->printusage());
    REQUIRE(!settings->printversion());
    REQUIRE(!settings->includearchives());
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
    REQUIRE(settings->paths()->empty());
}

TEST_CASE("Add extensions to FindSettings", "[FindSettings]") {
    auto *settings = new cppfind::FindSettings();

    REQUIRE(settings->in_archiveextensions()->empty());
    settings->add_in_archiveextension("zip,gz");
    REQUIRE(settings->in_archiveextensions()->size() == 2);

    REQUIRE(settings->out_archiveextensions()->empty());
    settings->add_out_archiveextension("rar,");
    REQUIRE(settings->out_archiveextensions()->size() == 1);

    REQUIRE(settings->in_extensions()->empty());
    settings->add_in_extension("cpp,h");
    REQUIRE(settings->in_extensions()->size() == 2);

    REQUIRE(settings->out_extensions()->empty());
    settings->add_out_extension("a,o");
    REQUIRE(settings->out_extensions()->size() == 2);
}

TEST_CASE("Add patterns to FindSettings", "[FindSettings]") {
    auto *settings = new cppfind::FindSettings();

    REQUIRE(settings->in_archivefilepatterns()->empty());
    settings->add_in_archivefilepattern("archive");
    REQUIRE(settings->in_archivefilepatterns()->size() == 1);

    REQUIRE(settings->out_archivefilepatterns()->empty());
    settings->add_out_archivefilepattern("old");
    REQUIRE(settings->out_archivefilepatterns()->size() == 1);

    REQUIRE(settings->in_dirpatterns()->empty());
    settings->add_in_dirpattern("dir");
    REQUIRE(settings->in_dirpatterns()->size() == 1);

    REQUIRE(settings->out_dirpatterns()->empty());
    settings->add_out_dirpattern("tmp");
    REQUIRE(settings->out_dirpatterns()->size() == 1);

    REQUIRE(settings->in_filepatterns()->empty());
    settings->add_in_filepattern("file");
    REQUIRE(settings->in_filepatterns()->size() == 1);

    REQUIRE(settings->out_filepatterns()->empty());
    settings->add_out_filepattern("stream");
    REQUIRE(settings->out_filepatterns()->size() == 1);
}

TEST_CASE("Alter booleans in FindSettings", "[FindSettings]") {
    auto *settings = new cppfind::FindSettings();

    REQUIRE(!settings->archivesonly());
    REQUIRE(!settings->includearchives());
    settings->archivesonly(true);
    REQUIRE(settings->archivesonly());
    REQUIRE(settings->includearchives());

    REQUIRE(!settings->debug());
    REQUIRE(!settings->verbose());
    settings->debug(true);
    REQUIRE(settings->debug());
    REQUIRE(settings->verbose());

    REQUIRE(settings->excludehidden());
    settings->excludehidden(false);
    REQUIRE(!settings->excludehidden());

    REQUIRE(!settings->listdirs());
    settings->listdirs(true);
    REQUIRE(settings->listdirs());

    REQUIRE(!settings->listfiles());
    settings->listfiles(true);
    REQUIRE(settings->listfiles());

    REQUIRE(!settings->printusage());
    settings->printusage(true);
    REQUIRE(settings->printusage());

    REQUIRE(!settings->printversion());
    settings->printversion(true);
    REQUIRE(settings->printversion());

    REQUIRE(settings->recursive());
    settings->recursive(false);
    REQUIRE(!settings->recursive());
}
