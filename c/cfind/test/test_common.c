#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"
#include "test_common.h"

void test_last_index_of_char_in_string(void)
{
    printf("\ntest_last_index_of_char_in_string()\n");

    char **filenames = (char *[]) {
        "filename.txt",
        "filename.",
        "filename",
        ".filename.txt",
        ".filename.",
        ".filename"
    };
    size_t arrlen = 6;
    int expected[6] = {
        8,
        8,
        -1,
        9,
        9,
        0
    };
    for (int i=0; i < arrlen; i++) {
        printf("filename: \"%s\"\n", filenames[i]);
        int res = last_index_of_char_in_string('.', filenames[i]);
        printf("expected res: %d\n", expected[i]);
        printf("actual res:   %d\n", res);
        assert(res == expected[i]);
    }
}
