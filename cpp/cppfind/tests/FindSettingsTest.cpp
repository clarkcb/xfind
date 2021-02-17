#include <catch2/catch.hpp>
#include "FindSettings.h"

TEST_CASE("Get default FindSettings", "[FindSettings]") {
    auto* settings = new cppfind::FindSettings();

    REQUIRE(!settings->archivesonly());
    REQUIRE(!settings->debug());
    REQUIRE(settings->excludehidden());
    REQUIRE(!settings->firstmatch());
    REQUIRE((settings->linesafter() == 0));
    REQUIRE((settings->linesbefore() == 0));
    REQUIRE(!settings->listdirs());
    REQUIRE(!settings->listfiles());
    REQUIRE(!settings->listlines());
    REQUIRE((settings->maxlinelength() == 150));
    REQUIRE(!settings->multilineoption-REMOVE());
    REQUIRE(!settings->printresults());
    REQUIRE(!settings->printusage());
    REQUIRE(!settings->printversion());
    REQUIRE(!settings->findarchives());
    REQUIRE(!settings->uniquelines());
    REQUIRE(!settings->verbose());

    REQUIRE(settings->startpath() == nullptr);

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
    REQUIRE(settings->findpatterns()->empty());
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
    REQUIRE(!settings->findarchives());
    settings->archivesonly(true);
    REQUIRE(settings->archivesonly());
    REQUIRE(settings->findarchives());

    REQUIRE(!settings->debug());
    REQUIRE(!settings->verbose());
    settings->debug(true);
    REQUIRE(settings->debug());
    REQUIRE(settings->verbose());

    REQUIRE(settings->excludehidden());
    settings->excludehidden(false);
    REQUIRE(!settings->excludehidden());

    REQUIRE(!settings->firstmatch());
    settings->firstmatch(true);
    REQUIRE(settings->firstmatch());

    REQUIRE(!settings->multilineoption-REMOVE());
    settings->multilineoption-REMOVE(true);
    REQUIRE(settings->multilineoption-REMOVE());

    REQUIRE(!settings->listdirs());
    settings->listdirs(true);
    REQUIRE(settings->listdirs());

    REQUIRE(!settings->listfiles());
    settings->listfiles(true);
    REQUIRE(settings->listfiles());

    REQUIRE(!settings->listlines());
    settings->listlines(true);
    REQUIRE(settings->listlines());

    REQUIRE(!settings->printresults());
    settings->printresults(true);
    REQUIRE(settings->printresults());

    REQUIRE(!settings->printusage());
    settings->printusage(true);
    REQUIRE(settings->printusage());

    REQUIRE(!settings->printversion());
    settings->printversion(true);
    REQUIRE(settings->printversion());

    REQUIRE(settings->recursive());
    settings->recursive(false);
    REQUIRE(!settings->recursive());

    REQUIRE(!settings->uniquelines());
    settings->uniquelines(true);
    REQUIRE(settings->uniquelines());
}

TEST_CASE("Alter ints in FindSettings", "[FindSettings]") {
    auto *settings = new cppfind::FindSettings();

    REQUIRE(settings->linesbefore() == 0);
    settings->linesbefore(5);
    REQUIRE(settings->linesbefore() == 5);

    // TODO: fix handling of negative values
    //m_settings->linesbefore(-5);
    //REQUIRE(m_settings->linesbefore() == 5);

    REQUIRE(settings->linesafter() == 0);
    settings->linesafter(5);
    REQUIRE(settings->linesafter() == 5);

    // TODO: fix handling of negative values
    //m_settings->linesafter(-5);
    //REQUIRE(m_settings->linesafter() == 5);
}
