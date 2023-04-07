#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "test_findsettings.h"

void test_default_settings(void)
{
    printf("\ntest_default_settings()\n");

    FindSettings *settings = default_settings();
    assert(settings->archivesonly == 0);
    assert(settings->debug == 0);
    assert(settings->excludehidden == 1);
    assert(settings->in_archiveextensions == NULL);
    assert(settings->in_archivefilepatterns == NULL);
    assert(settings->in_dirpatterns == NULL);
    assert(settings->in_extensions == NULL);
    assert(settings->in_filepatterns == NULL);
    assert(settings->in_filetypes == NULL);
    assert(settings->includearchives == 0);
    assert(settings->listdirs == 0);
    assert(settings->listfiles == 0);
    assert(settings->out_archiveextensions == NULL);
    assert(settings->out_archivefilepatterns == NULL);
    assert(settings->out_dirpatterns == NULL);
    assert(settings->out_extensions == NULL);
    assert(settings->out_filepatterns == NULL);
    assert(settings->out_filetypes == NULL);
    assert(settings->paths == NULL);
    assert(settings->printresults == 1);
    assert(settings->printusage == 0);
    assert(settings->printversion == 0);
    assert(settings->recursive == 1);
    assert(settings->sort_caseinsensitive == 0);
    assert(settings->sort_descending == 0);
    assert(settings->verbose == 0);
    destroy_settings(settings);
}
