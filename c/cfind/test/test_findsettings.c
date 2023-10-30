#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "test_findsettings.h"

void test_default_settings(void)
{
    printf("\ntest_default_settings()\n");

    FindSettings *settings = default_settings();
    assert(settings->archives_only == 0);
    assert(settings->debug == 0);
    assert(settings->in_archive_extensions == NULL);
    assert(settings->in_archive_file_patterns == NULL);
    assert(settings->in_dir_patterns == NULL);
    assert(settings->in_extensions == NULL);
    assert(settings->in_file_patterns == NULL);
    assert(settings->in_file_types == NULL);
    assert(settings->include_archives == 0);
    assert(settings->include_hidden == 0);
    assert(settings->list_dirs == 0);
    assert(settings->list_files == 0);
    assert(settings->max_last_mod == 0L);
    assert(settings->max_size == 0L);
    assert(settings->min_last_mod == 0L);
    assert(settings->min_size == 0L);
    assert(settings->out_archive_extensions == NULL);
    assert(settings->out_archive_file_patterns == NULL);
    assert(settings->out_dir_patterns == NULL);
    assert(settings->out_extensions == NULL);
    assert(settings->out_file_patterns == NULL);
    assert(settings->out_file_types == NULL);
    assert(settings->paths == NULL);
    assert(settings->print_results == 1);
    assert(settings->print_usage == 0);
    assert(settings->print_version == 0);
    assert(settings->recursive == 1);
    assert(settings->sort_case_insensitive == 0);
    assert(settings->sort_descending == 0);
    assert(settings->verbose == 0);
    destroy_settings(settings);
}
