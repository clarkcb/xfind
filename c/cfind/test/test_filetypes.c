#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "filetypes.h"
#include "test_filetypes.h"

void test_file_type_from_name(void)
{
    printf("\ntest_file_type_from_name()\n");

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
        FileType ft = file_type_from_name(names[i]);
        char ftname[8];
        file_type_to_name(expected[i], ftname);
        printf("expected ft: %s\n", ftname);
        file_type_to_name(ft, ftname);
        printf("actual ft:   %s\n", ftname);
        assert(ft == expected[i]);
    }
}

void test_get_file_type_for_ext(void)
{
    printf("\ntest_get_file_type_for_ext()\n");

    char **exts = (char *[]) {
        "zip",
        "mp3",
        "exe",
        "cpp",
        "ttf",
        "png",
        "txt",
        "mp4",
        "xml",
        "dunno"
    };
    size_t arrlen = 10;
    FileType expected[10] = {
        ARCHIVE,
        AUDIO,
        BINARY,
        CODE,
        FONT,
        IMAGE,
        TEXT,
        VIDEO,
        XML,
        UNKNOWN
    };
    FileTypes *file_types = new_file_types();
    get_file_types(file_types);
    for (int i=0; i < arrlen; i++) {
        printf("ext: \"%s\"\n", exts[i]);
        FileType ft = get_file_type_for_ext(exts[i], file_types);
        char ftname[8];
        file_type_to_name(expected[i], ftname);
        printf("expected ft: %s\n", ftname);
        file_type_to_name(ft, ftname);
        printf("actual ft:   %s\n", ftname);
        assert(ft == expected[i]);
    }
}

void test_is_file_type_for_ext(void)
{
    printf("\ntest_is_file_type_for_ext()\n");

    char **exts = (char *[]) {
        "zip",
        "mp3",
        "exe",
        "cpp",
        "ttf",
        "png",
        "txt",
        "mp4",
        "xml",
        "dunno"
    };
    size_t arrlen = 10;
    FileType expected[10] = {
        ARCHIVE,
        AUDIO,
        BINARY,
        CODE,
        FONT,
        IMAGE,
        TEXT,
        VIDEO,
        XML,
        UNKNOWN
    };
    FileTypes *file_types = new_file_types();
    get_file_types(file_types);
    for (int i=0; i < arrlen; i++) {
        printf("ext: \"%s\"\n", exts[i]);
        unsigned short expected_is_archive = expected[i] == ARCHIVE ? 1 : 0;
        unsigned short is_archive = is_archive_ext(exts[i], file_types);
        printf("expected_is_archive: %d\n", expected_is_archive);
        printf("is_archive: %d\n", is_archive);
        assert(is_archive == expected_is_archive);

        unsigned short expected_is_audio = expected[i] == AUDIO ? 1 : 0;
        unsigned short is_audio = is_audio_ext(exts[i], file_types);
        printf("expected_is_audio: %d\n", expected_is_audio);
        printf("is_audio: %d\n", is_audio);
        assert(is_audio == expected_is_audio);

        unsigned short expected_is_binary = expected[i] == BINARY ? 1 : 0;
        unsigned short is_binary = is_binary_ext(exts[i], file_types);
        printf("expected_is_binary: %d\n", expected_is_binary);
        printf("is_binary: %d\n", is_binary);
        assert(is_binary == expected_is_binary);

        unsigned short expected_is_code = expected[i] == CODE ? 1 : 0;
        unsigned short is_code = is_code_ext(exts[i], file_types);
        printf("expected_is_code: %d\n", expected_is_code);
        printf("is_code: %d\n", is_code);
        assert(is_code == expected_is_code);

        unsigned short expected_is_font = expected[i] == FONT ? 1 : 0;
        unsigned short is_font = is_font_ext(exts[i], file_types);
        printf("expected_is_font: %d\n", expected_is_font);
        printf("is_font: %d\n", is_font);
        assert(is_font == expected_is_font);

        unsigned short expected_is_image = expected[i] == IMAGE ? 1 : 0;
        unsigned short is_image = is_image_ext(exts[i], file_types);
        printf("expected_is_image: %d\n", expected_is_image);
        printf("is_image: %d\n", is_image);
        assert(is_image == expected_is_image);

        unsigned short expected_is_video = expected[i] == VIDEO ? 1 : 0;
        unsigned short is_video = is_video_ext(exts[i], file_types);
        printf("expected_is_video: %d\n", expected_is_video);
        printf("is_video: %d\n", is_video);
        assert(is_video == expected_is_video);

        unsigned short expected_is_xml = expected[i] == XML ? 1 : 0;
        unsigned short is_xml = is_xml_ext(exts[i], file_types);
        printf("expected_is_xml: %d\n", expected_is_xml);
        printf("is_xml: %d\n", is_xml);
        assert(is_xml == expected_is_xml);

        unsigned short expected_is_text = (is_code || is_xml || expected[i] == TEXT) ? 1 : 0;
        unsigned short is_text = is_text_ext(exts[i], file_types);
        printf("expected_is_text: %d\n", expected_is_text);
        printf("is_text: %d\n", is_text);
        assert(is_text == expected_is_text);

        unsigned short expected_is_unknown = expected[i] == UNKNOWN ? 1 : 0;
        unsigned short is_unknown = !is_archive && !is_audio && !is_binary && !is_code && !is_font && !is_image && !is_text && !is_video && !is_xml;
        printf("expected_is_unknown: %d\n", expected_is_unknown);
        printf("is_unknown: %d\n", is_unknown);
        assert(is_unknown == expected_is_unknown);
    }
}
