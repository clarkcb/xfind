#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include "finderr.h"
#include "test_findoptions.h"

#include "consolecolor.h"
#include "findoptions.h"

void test_settings_from_args(void)
{
    printf("\ntest_settings_from_args()\n");
    char **argv = (char *[]) {
        "-x",
        "c",
        "."
    };
    const int argc = 3;

    printf("argv: [\"-x\", \"c\", \".\"]\n");

    FindOptions *options = empty_find_options();
    error_t err = get_find_options(options);
    assert(err == E_OK);

    FindSettings *settings = default_settings();
    err = settings_from_args(argc, argv, options, settings);
    assert(err == E_OK);

    assert(settings->in_extensions != NULL);
    const int in_ext_count = (int)string_node_count(settings->in_extensions);
    const char* color = in_ext_count == 1 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sstring_node_count(settings->in_extensions): %d%s\n", color, in_ext_count, CONSOLE_COLOR_RESET);
    assert(in_ext_count == 1);
    const int in_ext_cmp = strcmp(settings->in_extensions->string, "c");
    color = in_ext_cmp == 0 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sstrcmp(settings->in_extensions->string, \"c\"): %d%s\n", color, in_ext_cmp, CONSOLE_COLOR_RESET);
    assert(in_ext_cmp == 0);

    assert(settings->paths != NULL);
    const int path_count = (int)path_node_count(settings->paths);
    color = path_count == 1 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%spath_node_count(settings->paths): %d%s\n", color, path_count, CONSOLE_COLOR_RESET);
    assert(path_count == 1);
    const Path *p = new_path(".");
    const int in_path_cmp = path_cmp(settings->paths->path, p);
    color = in_path_cmp == 0 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sstrcmp(settings->paths->dir, \".\"): %d%s\n", color, in_path_cmp, CONSOLE_COLOR_RESET);
    assert(in_path_cmp == 0);
}

void test_settings_from_json_string(void) {
    printf("\ntest_settings_from_json_string()\n");
    const char *json_string =
        "{\n"
        "  \"path\": \"~/src/xfind/typescript\",\n"
        "  \"in-ext\": [\"js\", \"ts\"],\n"
        "  \"out-dirpattern\": [\"build\", \"node_module\", \"test\", \"typing\"],\n"
        "  \"out-filepattern\": [\"gulpfile\", \"\\\\.min\\\\.\"],\n"
        "  \"debug\": true,\n"
        "  \"followsymlinks\": true,\n"
        "  \"includehidden\": false\n"
        "}";
    FindOptions *options = empty_find_options();
    error_t err = get_find_options(options);
    assert(err == E_OK);

    FindSettings *settings = default_settings();
    err = settings_from_json_string(json_string, options, settings);
    assert(err == E_OK);

    assert(settings->paths != NULL);
    const int path_count = (int)path_node_count(settings->paths);
    const char* color = path_count == 1 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%spath_node_count(settings->paths): %d%s\n", color, path_count, CONSOLE_COLOR_RESET);
    assert(path_count == 1);

    assert(settings->in_extensions != NULL);
    const int in_ext_count = (int)string_node_count(settings->in_extensions);
    color = in_ext_count == 2 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sstring_node_count(settings->in_extensions): %d%s\n", color, in_ext_count, CONSOLE_COLOR_RESET);
    assert(in_ext_count == 2);

    assert(settings->out_dir_patterns != NULL);
    const int out_dir_count = (int)regex_node_count(settings->out_dir_patterns);
    color = out_dir_count == 4 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sregex_node_count(settings->out_dir_patterns): %d%s\n", color, in_ext_count, CONSOLE_COLOR_RESET);
    assert(out_dir_count == 4);

    const bool debug = settings->debug;
    color = debug == true ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sdebug: %d%s\n", color, debug, CONSOLE_COLOR_RESET);
    assert(debug == true);

    const bool follow_symlinks = settings->follow_symlinks;
    color = follow_symlinks == true ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sfollow_symlinks: %d%s\n", color, follow_symlinks, CONSOLE_COLOR_RESET);
    assert(follow_symlinks == true);

    const bool include_hidden = settings->include_hidden;
    color = include_hidden == false ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sinclude_hidden: %d%s\n", color, include_hidden, CONSOLE_COLOR_RESET);
    assert(include_hidden == false);
}

// This is just for temporary testing
void test_settings_from_json_file(void) {
    printf("\ntest_settings_from_json_file()\n");
    const char *json_file = "/Users/cary/src/xfind/shared/settings.json";
    FindOptions *options = empty_find_options();
    error_t err = get_find_options(options);
    assert(err == E_OK);

    FindSettings *settings = default_settings();
    err = settings_from_json_file(json_file, options, settings);
    assert(err == E_OK);
}
