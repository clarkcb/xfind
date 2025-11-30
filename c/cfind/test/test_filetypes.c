#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "filetypes.h"
#include "test_filetypes.h"

#include "consolecolor.h"

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
    const size_t arrlen = 6;
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
        const FileType ft = file_type_from_name(names[i]);
        char ftname[8];
        file_type_to_name(expected[i], ftname);
        const char* color = CONSOLE_COLOR_GREEN;
        if (ft != expected[i]) {
            color = CONSOLE_COLOR_RED;
        }
        printf("%sexpected ft: %s%s\n", color, ftname, CONSOLE_COLOR_RESET);
        file_type_to_name(ft, ftname);
        printf("%sactual ft:   %s%s\n", color, ftname, CONSOLE_COLOR_RESET);
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
    const size_t arrlen = 10;
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
        const char* color = CONSOLE_COLOR_GREEN;
        if (ft != expected[i]) {
            color = CONSOLE_COLOR_RED;
        }
        printf("%sexpected ft: %s%s\n", color, ftname, CONSOLE_COLOR_RESET);
        file_type_to_name(ft, ftname);
        printf("%sactual ft:   %s%s\n", color, ftname, CONSOLE_COLOR_RESET);
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
    const size_t arrlen = 10;
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
        const char* color = is_archive == expected_is_archive ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
        printf("%sexpected_is_archive: %d%s\n", color, expected_is_archive, CONSOLE_COLOR_RESET);
        printf("%sis_archive: %d%s\n", color, is_archive, CONSOLE_COLOR_RESET);
        assert(is_archive == expected_is_archive);

        unsigned short expected_is_audio = expected[i] == AUDIO ? 1 : 0;
        unsigned short is_audio = is_audio_ext(exts[i], file_types);
        color = is_audio == expected_is_audio ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
        printf("%sexpected_is_audio: %d%s\n", color, expected_is_audio, CONSOLE_COLOR_RESET);
        printf("%sis_audio: %d%s\n", color, is_audio, CONSOLE_COLOR_RESET);
        assert(is_audio == expected_is_audio);

        unsigned short expected_is_binary = expected[i] == BINARY ? 1 : 0;
        unsigned short is_binary = is_binary_ext(exts[i], file_types);
        color = is_binary == expected_is_binary ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
        printf("%sexpected_is_binary: %d%s\n", color, expected_is_binary, CONSOLE_COLOR_RESET);
        printf("%sis_binary: %d%s\n", color, is_binary, CONSOLE_COLOR_RESET);
        assert(is_binary == expected_is_binary);

        unsigned short expected_is_code = expected[i] == CODE ? 1 : 0;
        unsigned short is_code = is_code_ext(exts[i], file_types);
        color = is_code == expected_is_code ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
        printf("%sexpected_is_code: %d%s\n", color, expected_is_code, CONSOLE_COLOR_RESET);
        printf("%sis_code: %d%s\n", color, is_code, CONSOLE_COLOR_RESET);
        assert(is_code == expected_is_code);

        unsigned short expected_is_font = expected[i] == FONT ? 1 : 0;
        unsigned short is_font = is_font_ext(exts[i], file_types);
        color = is_font == expected_is_font ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
        printf("%sexpected_is_font: %d%s\n", color, expected_is_font, CONSOLE_COLOR_RESET);
        printf("%sis_font: %d%s\n", color, is_font, CONSOLE_COLOR_RESET);
        assert(is_font == expected_is_font);

        unsigned short expected_is_image = expected[i] == IMAGE ? 1 : 0;
        unsigned short is_image = is_image_ext(exts[i], file_types);
        color = is_image == expected_is_image ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
        printf("%sexpected_is_image: %d%s\n", color, expected_is_image, CONSOLE_COLOR_RESET);
        printf("%sis_image: %d%s\n", color, is_image, CONSOLE_COLOR_RESET);
        assert(is_image == expected_is_image);

        unsigned short expected_is_video = expected[i] == VIDEO ? 1 : 0;
        unsigned short is_video = is_video_ext(exts[i], file_types);
        color = is_video == expected_is_video ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
        printf("%sexpected_is_video: %d%s\n", color, expected_is_video, CONSOLE_COLOR_RESET);
        printf("%sis_video: %d%s\n", color, is_video, CONSOLE_COLOR_RESET);
        assert(is_video == expected_is_video);

        unsigned short expected_is_xml = expected[i] == XML ? 1 : 0;
        unsigned short is_xml = is_xml_ext(exts[i], file_types);
        color = is_xml == expected_is_xml ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
        printf("%sexpected_is_xml: %d%s\n", color, expected_is_xml, CONSOLE_COLOR_RESET);
        printf("%sis_xml: %d%s\n", color, is_xml, CONSOLE_COLOR_RESET);
        assert(is_xml == expected_is_xml);

        unsigned short expected_is_text = (is_code || is_xml || expected[i] == TEXT) ? 1 : 0;
        unsigned short is_text = is_text_ext(exts[i], file_types);
        color = is_text == expected_is_text ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
        printf("%sexpected_is_text: %d%s\n", color, expected_is_text, CONSOLE_COLOR_RESET);
        printf("%sis_text: %d%s\n", color, is_text, CONSOLE_COLOR_RESET);
        assert(is_text == expected_is_text);

        unsigned short expected_is_unknown = expected[i] == UNKNOWN ? 1 : 0;
        unsigned short is_unknown = !is_archive && !is_audio && !is_binary && !is_code && !is_font && !is_image && !is_text && !is_video && !is_xml;
        color = is_unknown == expected_is_unknown ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
        printf("%sexpected_is_unknown: %d%s\n", color, expected_is_unknown, CONSOLE_COLOR_RESET);
        printf("%sis_unknown: %d%s\n", color, is_unknown, CONSOLE_COLOR_RESET);
        assert(is_unknown == expected_is_unknown);
    }
}
