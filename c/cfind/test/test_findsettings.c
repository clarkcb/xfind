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
    assert(settings->print_usage == 0);
    assert(settings->print_version == 0);
    assert(settings->recursive == 1);
    assert(settings->sort_case_insensitive == 0);
    assert(settings->sort_descending == 0);
    assert(settings->verbose == 0);
    destroy_settings(settings);
}

void test_add_extensions_to_settings(void)
{
    printf("\ntest_add_extensions_to_settings()\n");

    FindSettings *settings = default_settings();

    assert(settings->in_archive_extensions == NULL);
    settings->in_archive_extensions = new_string_node_from_char_split(',', "zip,gz");
    assert(settings->in_archive_extensions != NULL);
    size_t expected_count = 2;
    printf("in_archive_extensions expected_count: %lu\n", expected_count);
    size_t actual_count = string_node_count(settings->in_archive_extensions);
    printf("in_archive_extensions actual_count: %lu\n", actual_count);
    assert(expected_count == actual_count);

    assert(settings->out_archive_extensions == NULL);
//    printf("out_archive_extensions == NULL\n");
    settings->out_archive_extensions = new_string_node_from_char_split(',', ",rar");
    assert(settings->out_archive_extensions != NULL);
//    printf("out_archive_extensions != NULL\n");
    expected_count = 1;
    printf("out_archive_extensions expected_count: %lu\n", expected_count);
    actual_count = string_node_count(settings->out_archive_extensions);
    printf("out_archive_extensions actual_count: %lu\n", actual_count);
    assert(expected_count == actual_count);

    assert(settings->in_extensions == NULL);
    settings->in_extensions = new_string_node_from_char_split(',', "cpp,h");
    assert(settings->in_extensions != NULL);
    expected_count = 2;
    printf("in_extensions expected_count: %lu\n", expected_count);
    actual_count = string_node_count(settings->in_extensions);
    printf("in_extensions actual_count: %lu\n", actual_count);
    assert(expected_count == actual_count);

    assert(settings->out_extensions == NULL);
    settings->out_extensions = new_string_node_from_char_split(',', "a,o");
    assert(settings->out_extensions != NULL);
    expected_count = 2;
    printf("out_extensions expected_count: %lu\n", expected_count);
    actual_count = string_node_count(settings->out_extensions);
    printf("out_extensions actual_count: %lu\n", actual_count);
    assert(expected_count == actual_count);

    destroy_settings(settings);
}

void test_add_patterns_to_settings(void)
{
    printf("\ntest_add_patterns_to_settings()\n");

    FindSettings *settings = default_settings();

    assert(settings->in_archive_file_patterns == NULL);
    settings->in_archive_file_patterns = new_regex_node_from_string("foo");
    assert(settings->in_archive_file_patterns != NULL);
    add_string_to_regex_node("bar", settings->in_archive_file_patterns);
    assert(regex_node_count(settings->in_archive_file_patterns) == 2);

    assert(settings->out_archive_file_patterns == NULL);
    settings->out_archive_file_patterns = new_regex_node_from_string("baz");
    assert(settings->out_archive_file_patterns != NULL);
    assert(regex_node_count(settings->out_archive_file_patterns) == 1);

    assert(settings->in_dir_patterns == NULL);
    settings->in_dir_patterns = new_regex_node_from_string("foo");
    assert(settings->in_dir_patterns != NULL);
    add_string_to_regex_node("bar", settings->in_dir_patterns);
    assert(regex_node_count(settings->in_dir_patterns) == 2);

    assert(settings->out_dir_patterns == NULL);
    settings->out_dir_patterns = new_regex_node_from_string("baz");
    assert(settings->out_dir_patterns != NULL);
    assert(regex_node_count(settings->out_dir_patterns) == 1);

    assert(settings->in_file_patterns == NULL);
    settings->in_file_patterns = new_regex_node_from_string("foo");
    assert(settings->in_file_patterns != NULL);
    add_string_to_regex_node("bar", settings->in_file_patterns);
    assert(regex_node_count(settings->in_file_patterns) == 2);

    assert(settings->out_file_patterns == NULL);
    settings->out_file_patterns = new_regex_node_from_string("baz");
    assert(settings->out_file_patterns != NULL);
    assert(regex_node_count(settings->out_file_patterns) == 1);

    destroy_settings(settings);
}

void test_set_archives_only_in_settings(void)
{
    printf("\ntest_set_archives_only_in_settings()\n");

    FindSettings *settings = default_settings();

    assert(settings->archives_only == 0);
    set_archives_only(settings, 1);
    int expected_val = 1;
    assert(settings->archives_only == expected_val);
    assert(settings->include_archives == expected_val);

    destroy_settings(settings);
}

void test_set_debug_in_settings(void)
{
    printf("\ntest_set_debug_in_settings()\n");

    FindSettings *settings = default_settings();

    assert(settings->debug == 0);
    set_debug(settings, 1);
    int expected_val = 1;
    assert(settings->debug == expected_val);
    assert(settings->verbose == expected_val);

    destroy_settings(settings);
}
