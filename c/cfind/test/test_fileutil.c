#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "test_fileutil.h"

#include "color.h"

void test_dir_or_file_exists(void)
{
    printf("\ntest_dir_or_file_exists()\n");

    char xfindpath[MAX_HOMEPATH_LENGTH + 1];
    get_xfind_path(xfindpath);

    printf("dir: \"%s\"\n", xfindpath);
    const unsigned int res = dir_or_file_exists(xfindpath);
    const char* color = res == 1 ? COLOR_GREEN : COLOR_RED;
    printf("%sexpected exists: 1%s\n", color, COLOR_RESET);
    printf("%sactual exists:   %u%s\n", color, res, COLOR_RESET);
    assert(res == 1);

    const char *nonexistent = "/this/path/should/not/exist";
    printf("dir: \"%s\"\n", nonexistent);
    const unsigned int res2 = dir_or_file_exists(nonexistent);
    color = res2 == 0 ? COLOR_GREEN : COLOR_RED;
    printf("%sexpected exists: 0%s\n", color, COLOR_RESET);
    printf("%sactual exists:   %u%s\n", color, res2, COLOR_RESET);
    assert(res2 == 0);

    const char *tilde_home = "~/Documents";
    printf("dir: \"%s\"\n", tilde_home);
    const unsigned int res3 = dir_or_file_exists(tilde_home);
    color = res3 == 0 ? COLOR_GREEN : COLOR_RED;
    printf("%sexpected exists: 0%s\n", color, COLOR_RESET);
    printf("%sactual exists:   %u%s\n", color, res3, COLOR_RESET);
    assert(res3 == 0);

    char *expanded = malloc((strnlen(tilde_home, 100) + 1) * sizeof (char *));
    expanded[0] = '\0';
    expand_path(tilde_home, &expanded);
    printf("dir: \"%s\"\n", tilde_home);
    printf("expanded: \"%s\"\n", expanded);
    const unsigned int res4 = dir_or_file_exists(expanded);
    color = res4 == 1 ? COLOR_GREEN : COLOR_RED;
    printf("%sexpected exists: 1%s\n", color, COLOR_RESET);
    printf("%sactual exists:   %u%s\n", color, res4, COLOR_RESET);
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
        char *ext = malloc(4 * sizeof(char));
        get_extension(filenames[i], ext);
        const int res = strcmp(ext, exts[i]);
        const char* color = res == 0 ? COLOR_GREEN : COLOR_RED;
        printf("%sexpected ext: \"%s\"%s\n", color, exts[i], COLOR_RESET);
        printf("%sactual ext:   \"%s\"%s\n", color, ext, COLOR_RESET);
        assert(res == 0);
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
        const char* color = res == expected[i] ? COLOR_GREEN : COLOR_RED;
        printf("%sexpected res: %d%s\n", color, expected[i], COLOR_RESET);
        printf("%sactual res:   %d%s\n", color, res, COLOR_RESET);
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
        const char* color = res == expected[i] ? COLOR_GREEN : COLOR_RED;
        printf("%sexpected res: %d%s\n", color, expected[i], COLOR_RESET);
        printf("%sactual res:   %d%s\n", color, res, COLOR_RESET);
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
    const int diff = strnlen(expanded, 100) - strnlen(tilde, 100);
    const char* color = diff > 0 ? COLOR_GREEN : COLOR_RED;
    printf("%soriginal path: %s%s\n", color, tilde, COLOR_RESET);
    printf("%sexpanded path: %s%s\n", color, expanded, COLOR_RESET);
    assert(diff > 0);

    const char *homepath = "/home/path";
    expand_path(homepath, &expanded);
    const int diff2 = strnlen(expanded, 100) - strnlen(homepath, 100);
    color = diff2 == 0 ? COLOR_GREEN : COLOR_RED;
    printf("%soriginal path: %s%s\n", color, homepath, COLOR_RESET);
    printf("%sexpanded path: %s%s\n", color, expanded, COLOR_RESET);
    assert(diff2 == 0);
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
    const int cmp = strcmp(joined_path, "/home/path");
    const char* color = cmp == 0 ? COLOR_GREEN : COLOR_RED;
    printf("%sjoined_path: %s%s\n", color, joined_path, COLOR_RESET);
    assert(cmp == 0);

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
    const int dcmp = strcmp(d, "/home");
    const int fcmp = strcmp(f, "profile");
    const char* color = dcmp == 0 ? COLOR_GREEN : COLOR_RED;
    printf("%sd: %s%s\n", color, d, COLOR_RESET);
    color = fcmp == 0 ? COLOR_GREEN : COLOR_RED;
    printf("%sf: %s%s\n", color, f, COLOR_RESET);
    assert(strcmp(d, "/home") == 0);
    assert(strcmp(f, "profile") == 0);

    free(d);
    free(f);
}
