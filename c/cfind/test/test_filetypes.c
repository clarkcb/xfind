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
        char ftname[8];
        filetype_to_name(expected[i], ftname);
        printf("expected ft: %s\n", ftname);
        filetype_to_name(ft, ftname);
        printf("actual ft:   %s\n", ftname);
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
        char ftname[8];
        filetype_to_name(expected[i], ftname);
        printf("expected ft: %s\n", ftname);
        filetype_to_name(ft, ftname);
        printf("actual ft:   %s\n", ftname);
        assert(ft == expected[i]);
    }
}

void test_is_filetype_for_ext(void)
{
    printf("\ntest_is_filetype_for_ext()\n");

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
        unsigned short expected_is_archive = expected[i] == ARCHIVE ? 1 : 0;
        unsigned short is_archive = is_archive_ext(exts[i], fileTypes);
        printf("expected_is_archive: %d\n", expected_is_archive);
        printf("is_archive: %d\n", is_archive);
        assert(is_archive == expected_is_archive);

        unsigned short expected_is_binary = expected[i] == BINARY ? 1 : 0;
        unsigned short is_binary = is_binary_ext(exts[i], fileTypes);
        printf("expected_is_binary: %d\n", expected_is_binary);
        printf("is_binary: %d\n", is_binary);
        assert(is_binary == expected_is_binary);

        unsigned short expected_is_code = expected[i] == CODE ? 1 : 0;
        unsigned short is_code = is_code_ext(exts[i], fileTypes);
        printf("expected_is_code: %d\n", expected_is_code);
        printf("is_code: %d\n", is_code);
        assert(is_code == expected_is_code);

        unsigned short expected_is_xml = expected[i] == XML ? 1 : 0;
        unsigned short is_xml = is_xml_ext(exts[i], fileTypes);
        printf("expected_is_xml: %d\n", expected_is_xml);
        printf("is_xml: %d\n", is_xml);
        assert(is_xml == expected_is_xml);

        unsigned short expected_is_text = (is_code || is_xml || expected[i] == TEXT) ? 1 : 0;
        unsigned short is_text = is_text_ext(exts[i], fileTypes);
        printf("expected_is_text: %d\n", expected_is_text);
        printf("is_text: %d\n", is_text);
        assert(is_text == expected_is_text);

        unsigned short expected_is_unknown = expected[i] == UNKNOWN ? 1 : 0;
        unsigned short is_unknown = !is_archive && !is_binary && !is_code && !is_text && !is_xml;
        printf("expected_is_unknown: %d\n", expected_is_unknown);
        printf("is_unknown: %d\n", is_unknown);
        assert(is_unknown == expected_is_unknown);
    }
}
