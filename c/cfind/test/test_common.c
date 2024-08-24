#include <stdio.h>
#include <stdlib.h>
#include "test_common.h"

#include "color.h"

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
        printf("file_name: \"%s\"\n", filenames[i]);
        const int res = last_index_of_char_in_string('.', filenames[i]);
        const char* color = COLOR_GREEN;
        if (res != expected[i]) {
            color = COLOR_RED;
        }
        printf("%sexpected res: %d%s\n", color, expected[i], COLOR_RESET);
        printf("%sactual res:   %d%s\n", color, res, COLOR_RESET);
        assert(res == expected[i]);
    }
}
