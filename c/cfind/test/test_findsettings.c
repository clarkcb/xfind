#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include "test_findsettings.h"

#include "color.h"
#include "findsettings.h"

void test_default_settings(void)
{
    printf("\ntest_default_settings()\n");

    FindSettings *settings = default_settings();
    const char* color = settings->archives_only == false ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->archives_only: %d%s\n", color, settings->archives_only, COLOR_RESET);
    assert(settings->archives_only == false);

    color = settings->debug == false ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->debug: %d%s\n", color, settings->debug, COLOR_RESET);
    assert(settings->debug == false);

    color = settings->follow_symlinks == false ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->follow_symlinks: %d%s\n", color, settings->follow_symlinks, COLOR_RESET);
    assert(settings->follow_symlinks == false);

    const char* in_archive_extensions = settings->in_archive_extensions == NULL ? "NULL" : "NOT NULL";
    color = settings->in_archive_extensions == NULL ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->in_archive_extensions: %s%s\n", color, in_archive_extensions, COLOR_RESET);
    assert(settings->in_archive_extensions == NULL);

    const char* in_archive_file_patterns = settings->in_archive_file_patterns == NULL ? "NULL" : "NOT NULL";
    color = settings->in_archive_file_patterns == NULL ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->in_archive_file_patterns: %s%s\n", color, in_archive_file_patterns, COLOR_RESET);
    assert(settings->in_archive_file_patterns == NULL);

    const char* in_dir_patterns = settings->in_dir_patterns == NULL ? "NULL" : "NOT NULL";
    color = settings->in_dir_patterns == NULL ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->in_dir_patterns: %s%s\n", color, in_dir_patterns, COLOR_RESET);
    assert(settings->in_dir_patterns == NULL);

    const char* in_extensions = settings->in_extensions == NULL ? "NULL" : "NOT NULL";
    color = settings->in_extensions == NULL ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->in_extensions: %s%s\n", color, in_extensions, COLOR_RESET);
    assert(settings->in_extensions == NULL);

    const char* in_file_patterns = settings->in_file_patterns == NULL ? "NULL" : "NOT NULL";
    color = settings->in_file_patterns == NULL ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->in_file_patterns: %s%s\n", color, in_file_patterns, COLOR_RESET);
    assert(settings->in_file_patterns == NULL);

    const char* in_file_types = settings->in_file_types == NULL ? "NULL" : "NOT NULL";
    color = settings->in_file_types == NULL ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->in_file_types: %s%s\n", color, in_file_types, COLOR_RESET);
    assert(settings->in_file_types == NULL);

    color = settings->include_archives == false ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->include_archives: %d%s\n", color, settings->include_archives, COLOR_RESET);
    assert(settings->include_archives == false);

    color = settings->include_hidden == false ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->include_hidden: %d%s\n", color, settings->include_hidden, COLOR_RESET);
    assert(settings->include_hidden == false);

    color = settings->max_last_mod == 0L ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->max_last_mod: %ld%s\n", color, settings->max_last_mod, COLOR_RESET);
    assert(settings->max_last_mod == 0L);

    color = settings->max_size == 0L ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->max_size: %lu%s\n", color, settings->max_size, COLOR_RESET);
    assert(settings->max_size == 0L);

    color = settings->min_last_mod == 0L ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->min_last_mod: %ld%s\n", color, settings->min_last_mod, COLOR_RESET);
    assert(settings->min_last_mod == 0L);

    color = settings->min_size == 0L ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->min_size: %lu%s\n", color, settings->min_size, COLOR_RESET);
    assert(settings->min_size == 0L);

    const char* out_archive_extensions = settings->out_archive_extensions == NULL ? "NULL" : "NOT NULL";
    color = settings->out_archive_extensions == NULL ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->out_archive_extensions: %s%s\n", color, out_archive_extensions, COLOR_RESET);
    assert(settings->out_archive_extensions == NULL);

    const char* out_archive_file_patterns = settings->out_archive_file_patterns == NULL ? "NULL" : "NOT NULL";
    color = settings->out_archive_file_patterns == NULL ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->out_archive_file_patterns: %s%s\n", color, out_archive_file_patterns, COLOR_RESET);
    assert(settings->out_archive_file_patterns == NULL);

    const char* out_dir_patterns = settings->out_dir_patterns == NULL ? "NULL" : "NOT NULL";
    color = settings->out_dir_patterns == NULL ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->out_dir_patterns: %s%s\n", color, out_dir_patterns, COLOR_RESET);
    assert(settings->out_dir_patterns == NULL);

    const char* out_extensions = settings->out_extensions == NULL ? "NULL" : "NOT NULL";
    color = settings->out_extensions == NULL ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->out_extensions: %s%s\n", color, out_extensions, COLOR_RESET);
    assert(settings->out_extensions == NULL);

    const char* out_file_patterns = settings->out_file_patterns == NULL ? "NULL" : "NOT NULL";
    color = settings->out_file_patterns == NULL ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->out_file_patterns: %s%s\n", color, out_file_patterns, COLOR_RESET);
    assert(settings->out_file_patterns == NULL);

    const char* out_file_types = settings->out_file_types == NULL ? "NULL" : "NOT NULL";
    color = settings->out_file_types == NULL ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->out_file_types: %s%s\n", color, out_file_types, COLOR_RESET);
    assert(settings->out_file_types == NULL);

    const char* paths = settings->paths == NULL ? "NULL" : "NOT NULL";
    color = settings->paths == NULL ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->paths: %s%s\n", color, paths, COLOR_RESET);
    assert(settings->paths == NULL);

    color = settings->print_dirs == false ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->print_dirs: %d%s\n", color, settings->print_dirs, COLOR_RESET);
    assert(settings->print_dirs == false);

    color = settings->print_files == false ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->print_files: %d%s\n", color, settings->print_files, COLOR_RESET);
    assert(settings->print_files == false);

    color = settings->print_usage == false ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->print_usage: %d%s\n", color, settings->print_usage, COLOR_RESET);
    assert(settings->print_usage == false);

    color = settings->print_version == false ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->print_version: %d%s\n", color, settings->print_version, COLOR_RESET);
    assert(settings->print_version == false);

    color = settings->recursive == true ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->recursive: %d%s\n", color, settings->recursive, COLOR_RESET);
    assert(settings->recursive == true);

    color = settings->sort_case_insensitive == false ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->sort_case_insensitive: %d%s\n", color, settings->sort_case_insensitive, COLOR_RESET);
    assert(settings->sort_case_insensitive == false);

    color = settings->sort_descending == false ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->sort_descending: %d%s\n", color, settings->sort_descending, COLOR_RESET);
    assert(settings->sort_descending == false);

    color = settings->verbose == false ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->verbose: %d%s\n", color, settings->verbose, COLOR_RESET);
    assert(settings->verbose == false);

    destroy_settings(settings);
}

void test_add_extensions_to_settings(void)
{
    printf("\ntest_add_extensions_to_settings()\n");

    FindSettings *settings = default_settings();

    printf("Adding in-archive-extensions: \"zip,gz\"\n");
    assert(settings->in_archive_extensions == NULL);
    settings->in_archive_extensions = new_string_node_from_char_split(',', "zip,gz");
    assert(settings->in_archive_extensions != NULL);
    size_t expected_count = 2;
    size_t actual_count = string_node_count(settings->in_archive_extensions);
    const char* color = expected_count == actual_count ? COLOR_GREEN : COLOR_RED;
    printf("%sin_archive_extensions expected_count: %lu%s\n", color, expected_count, COLOR_RESET);
    printf("%sin_archive_extensions actual_count: %lu%s\n", color, actual_count, COLOR_RESET);
    assert(expected_count == actual_count);

    printf("Adding out-archive-extensions: \",rar\"\n");
    assert(settings->out_archive_extensions == NULL);
//    printf("out_archive_extensions == NULL\n");
    settings->out_archive_extensions = new_string_node_from_char_split(',', ",rar");
    assert(settings->out_archive_extensions != NULL);
//    printf("out_archive_extensions != NULL\n");
    expected_count = 1;
    actual_count = string_node_count(settings->out_archive_extensions);
    color = expected_count == actual_count ? COLOR_GREEN : COLOR_RED;
    printf("%sout_archive_extensions expected_count: %lu%s\n", color, expected_count, COLOR_RESET);
    printf("%sout_archive_extensions actual_count: %lu%s\n", color, actual_count, COLOR_RESET);
    assert(expected_count == actual_count);

    printf("Adding in-extensions: \"cpp,h\"\n");
    assert(settings->in_extensions == NULL);
    settings->in_extensions = new_string_node_from_char_split(',', "cpp,h");
    assert(settings->in_extensions != NULL);
    expected_count = 2;
    actual_count = string_node_count(settings->in_extensions);
    color = expected_count == actual_count ? COLOR_GREEN : COLOR_RED;
    printf("%sin_extensions expected_count: %lu%s\n", color, expected_count, COLOR_RESET);
    printf("%sin_extensions actual_count: %lu%s\n", color, actual_count, COLOR_RESET);
    assert(expected_count == actual_count);

    printf("Adding out-extensions: \"a,o\"\n");
    assert(settings->out_extensions == NULL);
    settings->out_extensions = new_string_node_from_char_split(',', "a,o");
    assert(settings->out_extensions != NULL);
    expected_count = 2;
    actual_count = string_node_count(settings->out_extensions);
    color = expected_count == actual_count ? COLOR_GREEN : COLOR_RED;
    printf("%sout_extensions expected_count: %lu%s\n", color, expected_count, COLOR_RESET);
    printf("%sout_extensions actual_count: %lu%s\n", color, actual_count, COLOR_RESET);
    assert(expected_count == actual_count);

    destroy_settings(settings);
}

void test_add_patterns_to_settings(void)
{
    printf("\ntest_add_patterns_to_settings()\n");

    FindSettings *settings = default_settings();

    printf("Adding in-archive-file-pattern: \"foo\"\n");
    assert(settings->in_archive_file_patterns == NULL);
    settings->in_archive_file_patterns = new_regex_node_from_string("foo");
    assert(settings->in_archive_file_patterns != NULL);
    printf("Adding in-archive-file-pattern: \"bar\"\n");
    add_string_to_regex_node("bar", settings->in_archive_file_patterns);
    const size_t res1 = regex_node_count(settings->in_archive_file_patterns);
    const char* color = res1 == 2 ? COLOR_GREEN : COLOR_RED;
    printf("%sregex_node_count(settings->in_archive_file_patterns): %lu%s\n", color, res1, COLOR_RESET);
    assert(res1 == 2);

    printf("Adding out-archive-file-pattern: \"baz\"\n");
    assert(settings->out_archive_file_patterns == NULL);
    settings->out_archive_file_patterns = new_regex_node_from_string("baz");
    assert(settings->out_archive_file_patterns != NULL);
    const size_t res2 = regex_node_count(settings->out_archive_file_patterns);
    color = res2 == 1 ? COLOR_GREEN : COLOR_RED;
    printf("%sregex_node_count(settings->out_archive_file_patterns): %lu%s\n", color, res2, COLOR_RESET);
    assert(res2 == 1);

    printf("Adding in-dir-pattern: \"foo\"\n");
    assert(settings->in_dir_patterns == NULL);
    settings->in_dir_patterns = new_regex_node_from_string("foo");
    assert(settings->in_dir_patterns != NULL);
    printf("Adding in-dir-pattern: \"bar\"\n");
    add_string_to_regex_node("bar", settings->in_dir_patterns);
    const size_t res3 = regex_node_count(settings->in_dir_patterns);
    color = res3 == 2 ? COLOR_GREEN : COLOR_RED;
    printf("%sregex_node_count(settings->in_dir_patterns): %lu%s\n", color, res3, COLOR_RESET);
    assert(res3 == 2);

    printf("Adding out-dir-pattern: \"baz\"\n");
    assert(settings->out_dir_patterns == NULL);
    settings->out_dir_patterns = new_regex_node_from_string("baz");
    assert(settings->out_dir_patterns != NULL);
    const size_t res4 = regex_node_count(settings->out_dir_patterns);
    color = res4 == 1 ? COLOR_GREEN : COLOR_RED;
    printf("%sregex_node_count(settings->out_dir_patterns): %lu%s\n", color, res4, COLOR_RESET);
    assert(res4 == 1);

    printf("Adding in-file-pattern: \"foo\"\n");
    assert(settings->in_file_patterns == NULL);
    settings->in_file_patterns = new_regex_node_from_string("foo");
    assert(settings->in_file_patterns != NULL);
    printf("Adding in-file-pattern: \"bar\"\n");
    add_string_to_regex_node("bar", settings->in_file_patterns);
    const size_t res5 = regex_node_count(settings->in_file_patterns);
    color = res5 == 2 ? COLOR_GREEN : COLOR_RED;
    printf("%sregex_node_count(settings->in_file_patterns): %lu%s\n", color, res5, COLOR_RESET);
    assert(res5 == 2);

    printf("Adding out-file-pattern: \"baz\"\n");
    assert(settings->out_file_patterns == NULL);
    settings->out_file_patterns = new_regex_node_from_string("baz");
    assert(settings->out_file_patterns != NULL);
    const size_t res6 = regex_node_count(settings->out_file_patterns);
    color = res6 == 1 ? COLOR_GREEN : COLOR_RED;
    printf("%sregex_node_count(settings->out_file_patterns): %lu%s\n", color, res6, COLOR_RESET);
    assert(res6 == 1);

    destroy_settings(settings);
}

void test_set_archives_only_in_settings(void)
{
    printf("\ntest_set_archives_only_in_settings()\n");

    FindSettings *settings = default_settings();

    assert(settings->archives_only == false);
    printf("set_archives_only(1)\n");
    set_archives_only(settings, 1);
    const int expected_val = 1;
    const char* color = settings->archives_only == expected_val ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->archives_only: %d%s\n", color, settings->archives_only, COLOR_RESET);
    assert(settings->archives_only == expected_val);
    color = settings->include_archives == expected_val ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->include_archives: %d%s\n", color, settings->include_archives, COLOR_RESET);
    assert(settings->include_archives == expected_val);

    destroy_settings(settings);
}

void test_set_debug_in_settings(void)
{
    printf("\ntest_set_debug_in_settings()\n");

    FindSettings *settings = default_settings();

    assert(settings->debug == false);
    printf("set_debug(1)\n");
    set_debug(settings, 1);
    const int expected_val = 1;
    const char* color = settings->debug == expected_val ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->debug: %d%s\n", color, settings->debug, COLOR_RESET);
    assert(settings->debug == expected_val);
    color = settings->verbose == expected_val ? COLOR_GREEN : COLOR_RED;
    printf("%ssettings->verbose: %d%s\n", color, settings->verbose, COLOR_RESET);
    assert(settings->verbose == expected_val);

    destroy_settings(settings);
}
