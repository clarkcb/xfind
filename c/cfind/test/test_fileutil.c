#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "test_fileutil.h"

void test_dir_or_file_exists(void)
{
    printf("\ntest_dir_or_file_exists()\n");

    char xfindpath[MAX_HOMEPATH_LENGTH + 1];
    get_xfindpath(xfindpath);

    printf("dir: \"%s\"\n", xfindpath);
    printf("expected exists: 1\n");
    unsigned int res = dir_or_file_exists(xfindpath);
    printf("actual exists:   %d\n", res);
    assert(res == 1);

    char *nonexistent = "/this/path/should/not/exist";
    printf("dir: \"%s\"\n", nonexistent);
    printf("expected exists: 0\n");
    unsigned int res2 = dir_or_file_exists(nonexistent);
    printf("actual exists:   %d\n", res2);
    assert(res2 == 0);

    char *tilde_home = "~/Documents";
    printf("dir: \"%s\"\n", tilde_home);
    printf("expected exists: 0\n");
    unsigned int res3 = dir_or_file_exists(tilde_home);
    printf("actual exists:   %d\n", res3);
    assert(res3 == 0);

    char *expanded = malloc((strlen(tilde_home) + 1) * sizeof (char *));
    expanded[0] = '\0';
    expand_path(tilde_home, &expanded);
    printf("dir: \"%s\"\n", tilde_home);
    printf("expanded: \"%s\"\n", expanded);
    printf("expected exists: 1\n");
    unsigned int res4 = dir_or_file_exists(expanded);
    printf("actual exists:   %d\n", res4);
    assert(res4 == 1);
    free(expanded);
}

void test_get_extension(void)
{
    printf("\ntest_get_extension()\n");

    const size_t arrlen = 6;
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
    for (int i=0; i < arrlen; i++) {
        printf("filename: \"%s\"\n", filenames[i]);
        char *ext = malloc(4 * sizeof(char *));
        get_extension(filenames[i], ext);
        printf("expected ext: \"%s\"\n", exts[i]);
        printf("actual ext:   \"%s\"\n", ext);
        assert(strcmp(ext, exts[i]) == 0);
    }
}

void test_is_dot_dir(void)
{
    printf("\ntest_is_dot_dir()\n");

    char **dirnames = (char *[]) {
        NULL,
        "",
        ".",
        "..",
        "./",
        "../",
        ".gitignore",
        "./some/relative/path",
        "../some/parent/path",
        "/some/root/path"
    };
    const size_t arrlen = 10;
    int expected[10] = {
        0,
        0,
        1,
        1,
        1,
        1,
        0,
        0,
        0,
        0
    };

    for (int i=0; i < arrlen; i++) {
        printf("dir: \"%s\"\n", dirnames[i]);
        int res = is_dot_dir(dirnames[i]);
        printf("expected res: %d\n", expected[i]);
        printf("actual res:   %d\n", res);
        assert(res == expected[i]);
    }
}

void test_is_hidden(void)
{
    printf("\ntest_is_hidden()\n");

    char **filenames = (char *[]) {
        NULL,
        "",
        ".",
        "..",
        "./",
        "../",
        ".gitignore",
        "filename.txt",
        ".filename.txt",
        "./filename.txt",
        "./.filename.txt",
        "../filename.txt",
        "../.filename.txt",
        "/filename.txt",
        "/.filename.txt",
        "/path/to/a/filename.txt",
        "/path/to/a/.filename.txt"
    };
    const size_t arrlen = 17;
    int expected[17] = {
        0,
        0,
        0,
        0,
        0,
        0,
        1,
        0,
        1,
        0,
        1,
        0,
        1,
        0,
        1,
        0,
        1
    };

    for (int i=0; i < arrlen; i++) {
        printf("filename: \"%s\"\n", filenames[i]);
        int res = is_hidden(filenames[i]);
        printf("expected res: %d\n", expected[i]);
        printf("actual res:   %d\n", res);
        assert(res == expected[i]);
    }
}

void test_expand_path(void)
{
    printf("\ntest_expand_path()\n");

    char *expanded = (char *)malloc(50);
    expanded[0] = '\0';

    char *tilde = "~";
    expand_path(tilde, &expanded);
    printf("original path: %s\n", tilde);
    printf("expanded path: %s\n", expanded);
    assert(strlen(expanded) > strlen(tilde));

    char *homepath = "/home/path";
    expand_path(homepath, &expanded);
    printf("original path: %s\n", homepath);
    printf("expanded path: %s\n", expanded);
    assert(strlen(expanded) == strlen(homepath));
    assert(strcmp(expanded, homepath) == 0);

    free(expanded);
}

void test_join_path(void)
{
    printf("\ntest_join_path()\n");

    char *joined_path = (char *)malloc(100 * sizeof(char));

    char *path1 = "/home";
    char *path2 = "path";

    join_path(path1, path2, joined_path);
    printf("path1: %s\n", path1);
    printf("path2: %s\n", path2);
    printf("joined_path: %s\n", joined_path);
    assert(strcmp(joined_path, "/home/path") == 0);

    // TODO: fix - need to normalize paths before joining
//    path1 = "/home/";
//
//    join_path(path1, path2, joined_path);
//    printf("path1: %s\n", path1);
//    printf("path2: %s\n", path2);
//    printf("joined_path: %s\n", joined_path);
//    assert(strcmp(joined_path, "/home/path") == 0);

    free(joined_path);
}

void test_split_path(void)
{
    printf("\ntest_split_path()\n");

    char *path = "/home/profile";

    char *d = (char *)malloc(50);
    char *f = (char *)malloc(50);
    split_path(path, &d, &f);
    printf("path: %s\n", path);
    printf("d: %s\n", d);
    printf("f: %s\n", f);
    assert(strcmp(d, "/home") == 0);
    assert(strcmp(f, "profile") == 0);

    free(d);
    free(f);
}
