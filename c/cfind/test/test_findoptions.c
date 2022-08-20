#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "finderr.h"
#include "test_findoptions.h"

void test_settings_from_args(void)
{
    printf("\ntest_settings_from_args()\n");
    char **argv = (char *[]) {
        "-x",
        "c",
        "."
    };
    int argc = 3;

    FindSettings *settings = default_settings();
    error_t err = settings_from_args(argc, argv, settings);
    assert(err == E_OK);

    assert(settings->in_extensions != NULL);
    assert(string_node_count(settings->in_extensions) == 1);
    assert(strcmp(settings->in_extensions->string, "c") == 0);

    assert(settings->paths != NULL);
    assert(string_node_count(settings->paths) == 1);
    assert(strcmp(settings->paths->string, ".") == 0);
}
