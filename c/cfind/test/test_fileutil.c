#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "test_fileutil.h"

void test_dir_or_file_exists(void)
{
    printf("\ntest_dir_or_file_exists()\n");

    char xfindpath[MAX_HOMEPATH_LENGTH + 1];
    get_xfind_path(xfindpath);

    printf("dir: \"%s\"\n", xfindpath);
    printf("expected exists: 1\n");
    const unsigned int res = dir_or_file_exists(xfindpath);
    printf("actual exists:   %u\n", res);
    assert(res == 1);

    const char *nonexistent = "/this/path/should/not/exist";
    printf("dir: \"%s\"\n", nonexistent);
    printf("expected exists: 0\n");
    const unsigned int res2 = dir_or_file_exists(nonexistent);
    printf("actual exists:   %u\n", res2);
    assert(res2 == 0);

    const char *tilde_home = "~/Documents";
    printf("dir: \"%s\"\n", tilde_home);
    printf("expected exists: 0\n");
    const unsigned int res3 = dir_or_file_exists(tilde_home);
    printf("actual exists:   %u\n", res3);
    assert(res3 == 0);

    char *expanded = malloc((strnlen(tilde_home, 100) + 1) * sizeof (char *));
    expanded[0] = '\0';
    expand_path(tilde_home, &expanded);
    printf("dir: \"%s\"\n", tilde_home);
    printf("expanded: \"%s\"\n", expanded);
    printf("expected exists: 1\n");
    const unsigned int res4 = dir_or_file_exists(expanded);
    printf("actual exists:   %u\n", res4);
    assert(res4 == 1);
    free(expanded);
}

void test_get_extension(void)
{
    printf("\ntest_get_extension()\n");

    const size_t arrlen = 6;
    char **filenames = (char *[]) {
        "file_name.txt",
        "file_name.",
        "file_name",
        ".file_name.txt",
        ".file_name.",
        ".file_name"
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
        printf("file_name: \"%s\"\n", filenames[i]);
        char *ext = malloc(4 * sizeof(char *));
        get_extension(filenames[i], ext);
        printf("expected ext: \"%s\"\n", exts[i]);
        printf("actual ext:   \"%s\"\n", ext);
        assert(strcmp(ext, exts[i]) == 0);
        free(ext);
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
    const int expected[10] = {
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
        const int res = is_dot_dir(dirnames[i]);
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
        "file_name.txt",
        ".file_name.txt",
        "./file_name.txt",
        "./.file_name.txt",
        "../file_name.txt",
        "../.file_name.txt",
        "/file_name.txt",
        "/.file_name.txt",
        "/path/to/a/file_name.txt",
        "/path/to/a/.file_name.txt"
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
        printf("file_name: \"%s\"\n", filenames[i]);
        const int res = is_hidden(filenames[i]);
        printf("expected res: %d\n", expected[i]);
        printf("actual res:   %d\n", res);
        assert(res == expected[i]);
    }
}

void test_expand_path(void)
{
    printf("\ntest_expand_path()\n");

    char *expanded = malloc(50);
    expanded[0] = '\0';

    const char *tilde = "~";
    expand_path(tilde, &expanded);
    printf("original path: %s\n", tilde);
    printf("expanded path: %s\n", expanded);
    assert(strnlen(expanded, 100) > strnlen(tilde, 100));

    const char *homepath = "/home/path";
    expand_path(homepath, &expanded);
    printf("original path: %s\n", homepath);
    printf("expanded path: %s\n", expanded);
    assert(strnlen(expanded, 100) == strnlen(homepath, 100));
    assert(strcmp(expanded, homepath) == 0);

    free(expanded);
}

void test_join_path(void)
{
    printf("\ntest_join_path()\n");

    char *joined_path = malloc(100 * sizeof(char));

    const char *path1 = "/home";
    const char *path2 = "path";

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

    const char *path = "/home/profile";

    char *d = malloc(50);
    char *f = malloc(50);
    split_path(path, &d, &f);
    printf("path: %s\n", path);
    printf("d: %s\n", d);
    printf("f: %s\n", f);
    assert(strcmp(d, "/home") == 0);
    assert(strcmp(f, "profile") == 0);

    free(d);
    free(f);
}
