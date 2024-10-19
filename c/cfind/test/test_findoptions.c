#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "finderr.h"
#include "test_findoptions.h"

#include "color.h"

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

    FindSettings *settings = default_settings();
    const error_t err = settings_from_args(argc, argv, settings);
    assert(err == E_OK);

    assert(settings->in_extensions != NULL);
    const int in_ext_count = (int)string_node_count(settings->in_extensions);
    const char* color = in_ext_count == 1 ? COLOR_GREEN : COLOR_RED;
    printf("%sstring_node_count(settings->in_extensions): %d%s\n", color, in_ext_count, COLOR_RESET);
    assert(in_ext_count == 1);
    const int in_ext_cmp = strcmp(settings->in_extensions->string, "c");
    color = in_ext_cmp == 0 ? COLOR_GREEN : COLOR_RED;
    printf("%sstrcmp(settings->in_extensions->string, \"c\"): %d%s\n", color, in_ext_cmp, COLOR_RESET);
    assert(in_ext_cmp == 0);

    assert(settings->paths != NULL);
    const int path_count = (int)path_node_count(settings->paths);
    color = path_count == 1 ? COLOR_GREEN : COLOR_RED;
    printf("%spath_node_count(settings->paths): %d%s\n", color, path_count, COLOR_RESET);
    assert(path_count == 1);
    const Path *p = new_path(".");
    const int in_path_cmp = path_cmp(settings->paths->path, p);
    color = in_path_cmp == 0 ? COLOR_GREEN : COLOR_RED;
    printf("%sstrcmp(settings->paths->dir, \".\"): %d%s\n", color, in_path_cmp, COLOR_RESET);
    assert(in_path_cmp == 0);
}
