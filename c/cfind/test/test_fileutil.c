#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "test_fileutil.h"

void test_dir_or_file_exists(void)
{
    printf("\ntest_dir_or_file_exists()\n");

    char **filenames = (char *[]) {
        "filename.txt",
        "filename.",
        "filename",
        ".filename.txt",
        ".filename.",
        ".filename"
    };
    char **exts = (char *[]) {
        "txt",
        "",
        "",
        "txt",
        "",
        ""
    };
    size_t arrlen = 6;
    for (int i=0; i < arrlen; i++) {
        printf("filename: \"%s\"\n", filenames[i]);
        char *ext = malloc(sizeof(char *));
        get_extension(filenames[i], ext);
        printf("expected ext: \"%s\"\n", exts[i]);
        printf("actual ext:   \"%s\"\n", ext);
        assert(strcmp(ext, exts[i]) == 0);
    }

}

void test_is_hidden(void)
{
    printf("\ntest_is_hidden()\n");

    char **filenames = (char *[]) {
        ".",
        "..",
        "./",
        "../",
        ".gitignore",
        "filename.txt",
        ".filename.txt",
        "./filename.txt"
    };
    int expected[8] = {
        0,
        0,
        0,
        0,
        1,
        0,
        1,
        0
    };
    size_t arrlen = 8;
    for (int i=0; i < arrlen; i++) {
        printf("filename: \"%s\"\n", filenames[i]);
        int res = is_hidden(filenames[i]);
        printf("expected res: %d\n", expected[i]);
        printf("actual res:   %d\n", res);
        assert(res == expected[i]);
    }

}
