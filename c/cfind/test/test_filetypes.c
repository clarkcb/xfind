#include <assert.h>
#include <stdio.h>
#include <string.h>
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

void test_get_filetype_for_ext(void)
{
    printf("\ntest_get_filetype_for_ext()\n");

    char **exts = (char *[]) {
        "zip",
        "exe",
        "cpp",
        "txt",
        "xml",
        "dunno"
    };
    size_t arrlen = 6;
    FileType expected[6] = {
        ARCHIVE,
        BINARY,
        CODE,
        TEXT,
        XML,
        UNKNOWN
    };
    FileTypes *fileTypes = new_filetypes();
    get_filetypes(fileTypes);
    for (int i=0; i < arrlen; i++) {
        printf("ext: \"%s\"\n", exts[i]);
        FileType ft = get_filetype_for_ext(exts[i], fileTypes);
        printf("expected ft: %d\n", expected[i]);
        printf("actual ft:   %d\n", ft);
        assert(ft == expected[i]);
    }
}
