#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"
#include "filetypes.h"
#include "test_filetypes.h"

void test_filetype_from_name(void)
{
    printf("\ntest_filetype_from_name()\n");

    char **names = (char *[]) {
        "TEXT",
        "Text",
        "text",
        "BINARY",
        "Binary",
        "binary"
    };
    size_t arrlen = 6;
    FileType expected[6] = {
        TEXT,
        TEXT,
        TEXT,
        BINARY,
        BINARY,
        BINARY
    };
    for (int i=0; i < arrlen; i++) {
        printf("name: \"%s\"\n", names[i]);
        FileType ft = filetype_from_name(names[i]);
        printf("expected ft: %d\n", expected[i]);
        printf("actual ft:   %d\n", ft);
        assert(ft == expected[i]);
    }

}
