#include <stdio.h>

#include "test_finder.h"

#include <assert.h>
#include <string.h>
#include <sys/stat.h>

#include "consolecolor.h"
#include "config.h"
#include "finder.h"
#include "finderr.h"
#include "findsettings.h"


void test_validate_settings(void)
{
    printf("\ntest_validate_settings()\n");

    FindSettings *settings = default_settings();
    int empty_paths = is_null_or_empty_path_node(settings->paths);
    const char* color = empty_paths == 1 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_null_or_empty_string_node(settings->paths): %d%s\n", color, empty_paths, CONSOLE_COLOR_RESET);
    assert(empty_paths == 1);

    error_t err = validate_settings(settings);
    color = err == E_STARTPATH_NOT_DEFINED ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    if (err == E_STARTPATH_NOT_DEFINED) {
        printf("%svalidate_settings(settings): E_STARTPATH_NOT_DEFINED%s\n", color, CONSOLE_COLOR_RESET);
    }
    assert(err == E_STARTPATH_NOT_DEFINED);

    // Add paths
    const char* f1 = "./non_existent_file.h";
    printf("Adding path: \"%s\"\n", f1);
    Path *p1 = new_path(f1);
    settings->paths = new_path_node(p1);
    empty_paths = is_null_or_empty_path_node(settings->paths);
    assert(empty_paths == 0);
    size_t path_count = path_node_count(settings->paths);
    color = path_count == 1 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%spath_node_count(settings->paths): %zu%s\n", color, path_count, CONSOLE_COLOR_RESET);
    assert(path_count == 1);

    const char* f2 = "./non_existent_file.c";
    printf("Adding path: \"%s\"\n", f2);
    Path *p2 = new_path(f2);
    add_path_to_path_node(p2, settings->paths);
    path_count = path_node_count(settings->paths);
    color = path_count == 2 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%spath_node_count(settings->paths): %zu%s\n", color, path_count, CONSOLE_COLOR_RESET);
    assert(path_count == 2);

    err = validate_settings(settings);
    color = err == E_STARTPATH_NOT_FOUND ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    if (err == E_STARTPATH_NOT_FOUND) {
        printf("%svalidate_settings(settings): E_STARTPATH_NOT_FOUND%s\n", color, CONSOLE_COLOR_RESET);
    }
    assert(err == E_STARTPATH_NOT_FOUND);

    destroy_settings(settings);
}

void test_is_matching_dir(void) {
    printf("\ntest_is_matching_dir()\n");

    FindSettings *settings = default_settings();
    // const char* f = ".";
    // Path *p1 = new_path(f);
    // settings->paths = new_path_node(p1);

    // test current dot dir
    const char* dot_dir = ".";
    const unsigned short res1 = is_matching_dir(settings, dot_dir);
    const char* color = res1 == 1 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_dir(\"%s\"): %d%s\n", color, dot_dir, res1, CONSOLE_COLOR_RESET);
    assert(res1 == 1);

    // test parent dot dir
    const char* parent_dir = "..";
    const unsigned short res2 = is_matching_dir(settings, parent_dir);
    color = res2 == 1 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_dir(\"%s\"): %d%s\n", color, parent_dir, res2, CONSOLE_COLOR_RESET);
    assert(res2 == 1);

    // test "test" dir
    const char* test_dir = "test";
    const unsigned short res3 = is_matching_dir(settings, test_dir);
    color = res3 == 1 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_dir(\"%s\"): %d%s\n", color, test_dir, res3, CONSOLE_COLOR_RESET);
    assert(res3 == 1);

    destroy_settings(settings);
}

void test_is_matching_dir_in_dir_patterns(void) {
    printf("\ntest_is_matching_dir_in_dir_patterns()\n");

    FindSettings *settings = default_settings();
    Path *p = new_path(".");
    settings->paths = new_path_node(p);

    // test "test" dir with "test" in_dir_pattern
    const char* test_dir = "test";
    printf("Adding in-dir-pattern: \"%s\"\n", test_dir);
    settings->in_dir_patterns = new_regex_node_from_string("test");
    const unsigned short res1 = is_matching_dir(settings, test_dir);
    const char* color = res1 == 1 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_dir(\"%s\"): %d%s\n", color, test_dir, res1, CONSOLE_COLOR_RESET);
    assert(res1 == 1);

    // test "other" dir with "test" in_dir_pattern
    const char* other_dir = "other";
    const unsigned short res2 = is_matching_dir(settings, other_dir);
    color = res2 == 0 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_dir(\"%s\"): %d%s\n", color, other_dir, res2, CONSOLE_COLOR_RESET);
    assert(res2 == 0);

    destroy_settings(settings);
}

void test_is_matching_dir_out_dir_patterns(void) {
    printf("\ntest_is_matching_dir_out_dir_patterns()\n");

    FindSettings *settings = default_settings();
    Path *p = new_path(".");
    settings->paths = new_path_node(p);

    // test "test" dir with "test" out_dir_pattern
    const char* test_dir = "test";
    printf("Adding out-dir-pattern: \"%s\"\n", test_dir);
    settings->out_dir_patterns = new_regex_node_from_string("test");
    const unsigned short res1 = is_matching_dir(settings, test_dir);
    const char* color = res1 == 0 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_dir(\"%s\"): %d%s\n", color, test_dir, res1, CONSOLE_COLOR_RESET);
    assert(res1 == 0);

    // test "other" dir with "test" out_dir_pattern
    const char* other_dir = "other";
    const unsigned short res2 = is_matching_dir(settings, other_dir);
    color = res2 == 1 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_dir(\"%s\"): %d%s\n", color, test_dir, res2, CONSOLE_COLOR_RESET);
    assert(res2 == 1);

    destroy_settings(settings);
}

void test_filter_path(void) {
    printf("\ntest_filter_path()\n");

    FindSettings *settings = default_settings();
    Path *p = new_path(".");
    settings->paths = new_path_node(p);

    FileTypes *file_types = new_file_types();
    const error_t err = init_file_types(file_types);
    assert(err == E_OK);

    const char* test_file = "./test_finder.c";
    const Path *test_path = new_path(test_file);
    const FileType ft = CODE;
    const unsigned short res1 = filter_path(settings, test_path, &ft, 0, 0);
    const char* color = res1 == 1 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sfilter_path(\"%s\"): %d%s\n", color, test_file, res1, CONSOLE_COLOR_RESET);
    assert(res1 == 1);

    const char* hidden_file = "./.hidden.c";
    const Path *hidden_path = new_path(hidden_file);
    const unsigned short res2 = filter_path(settings, hidden_path, &ft, 0, 0);
    color = res2 == 0 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sfilter_path(\"%s\"): %d%s\n", color, hidden_file, res2, CONSOLE_COLOR_RESET);
    assert(res2 == 0);

    destroy_settings(settings);
}

void test_is_matching_path_in_extensions(void) {
    printf("\ntest_is_matching_path_in_extensions()\n");

    FindSettings *settings = default_settings();
    Path *p = new_path(".");
    settings->paths = new_path_node(p);
    printf("Adding in-extension: \"c\"\n");
    settings->in_extensions = new_string_node("c");

    FileTypes *file_types = new_file_types();
    const error_t err = init_file_types(file_types);
    assert(err == E_OK);

    const char* matching_file = "test_finder.c";
    const Path *matching_path = new_path(matching_file);
    const FileType ft = CODE;
    const unsigned short res1 = is_matching_path(settings, matching_path, &ft, 0, 0);
    const char* color = res1 == 1 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_path(\"%s\"): %d%s\n", color, matching_file, res1, CONSOLE_COLOR_RESET);
    assert(res1 == 1);

    const char* non_matching_file = "test_finder.h";
    const Path *non_matching_path = new_path(non_matching_file);
    const unsigned short res2 = is_matching_path(settings, non_matching_path, &ft, 0, 0);
    color = res2 == 0 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_path(\"%s\"): %d%s\n", color, non_matching_file, res2, CONSOLE_COLOR_RESET);
    assert(res2 == 0);

    destroy_settings(settings);
}

void test_is_matching_path_out_extensions(void) {
    printf("\ntest_is_matching_path_out_extensions()\n");

    FindSettings *settings = default_settings();
    Path *p = new_path(".");
    settings->paths = new_path_node(p);
    printf("Adding out-extension: \"c\"\n");
    settings->out_extensions = new_string_node("c");

    FileTypes *file_types = new_file_types();
    const error_t err = init_file_types(file_types);
    assert(err == E_OK);

    const char* matching_file = "test_finder.c";
    const Path *matching_path = new_path(matching_file);
    const FileType ft = CODE;
    const unsigned short res1 = is_matching_path(settings, matching_path, &ft, 0, 0);
    const char* color = res1 == 0 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_path(\"%s\"): %d%s\n", color, matching_file, res1, CONSOLE_COLOR_RESET);
    assert(res1 == 0);

    const char* non_matching_file = "test_finder.h";
    const Path *non_matching_path = new_path(non_matching_file);
    const unsigned short res2 = is_matching_path(settings, non_matching_path, &ft, 0, 0);
    color = res2 == 1 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_path(\"%s\"): %d%s\n", color, non_matching_file, res2, CONSOLE_COLOR_RESET);
    assert(res2 == 1);

    destroy_settings(settings);
}

void test_is_matching_path_in_file_patterns(void) {
    printf("\ntest_is_matching_path_in_file_patterns()\n");

    FindSettings *settings = default_settings();
    Path *p = new_path(".");
    settings->paths = new_path_node(p);
    printf("Adding in-file-pattern: \"test\"\n");
    settings->in_file_patterns = new_regex_node_from_string("test");

    FileTypes *file_types = new_file_types();
    const error_t err = init_file_types(file_types);
    assert(err == E_OK);

    const char* matching_file = "test_finder.c";
    const Path *matching_path = new_path(matching_file);
    const FileType ft = CODE;
    const unsigned short res1 = is_matching_path(settings, matching_path, &ft, 0, 0);
    const char* color = res1 == 1 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_path(\"%s\"): %d%s\n", color, matching_file, res1, CONSOLE_COLOR_RESET);
    assert(res1 == 1);

    const char* non_matching_file = "finder.c";
    const Path *non_matching_path = new_path(non_matching_file);
    const unsigned short res2 = is_matching_path(settings, non_matching_path, &ft, 0, 0);
    color = res2 == 0 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_path(\"%s\"): %d%s\n", color, non_matching_file, res2, CONSOLE_COLOR_RESET);
    assert(res2 == 0);

    destroy_settings(settings);
}

void test_is_matching_path_out_file_patterns(void) {
    printf("\ntest_is_matching_path_out_file_patterns()\n");

    FindSettings *settings = default_settings();
    Path *p = new_path(".");
    settings->paths = new_path_node(p);
    printf("Adding out-file-pattern: \"test\"\n");
    settings->out_file_patterns = new_regex_node_from_string("test");

    FileTypes *file_types = new_file_types();
    const error_t err = init_file_types(file_types);
    assert(err == E_OK);

    const char* matching_file = "test_finder.c";
    const Path *matching_path = new_path(matching_file);
    const FileType ft = CODE;
    const unsigned short res1 = is_matching_path(settings, matching_path, &ft, 0, 0);
    const char* color = res1 == 0 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_path(\"%s\"): %d%s\n", color, matching_file, res1, CONSOLE_COLOR_RESET);
    assert(res1 == 0);

    const char* non_matching_file = "finder.c";
    const Path *non_matching_path = new_path(non_matching_file);
    const unsigned short res2 = is_matching_path(settings, non_matching_path, &ft, 0, 0);
    color = res2 == 1 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_path(\"%s\"): %d%s\n", color, non_matching_file, res2, CONSOLE_COLOR_RESET);
    assert(res2 == 1);

    destroy_settings(settings);
}

void test_is_matching_path_in_file_types(void) {
    printf("\ntest_is_matching_path_in_file_types()\n");

    FindSettings *settings = default_settings();
    Path *p = new_path(".");
    settings->paths = new_path_node(p);
    const FileType file_type = CODE;
    int *ftint = malloc(sizeof(int));
    *ftint = (int)file_type;
    printf("Adding in-file-type: CODE\n");
    settings->in_file_types = empty_int_node();
    add_int_to_int_node(ftint, settings->in_file_types);

    FileTypes *file_types = new_file_types();
    const error_t err = init_file_types(file_types);
    assert(err == E_OK);

    const char* matching_file = "finder.c";
    const Path *matching_path = new_path(matching_file);
    FileType ft = CODE;
    const unsigned short res1 = is_matching_path(settings, matching_path, &ft, 0, 0);
    const char* color = res1 == 1 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_path(\"%s\"): %d%s\n", color, matching_file, res1, CONSOLE_COLOR_RESET);
    assert(res1 == 1);

    const char* non_matching_file = "README.md";
    const Path *non_matching_path = new_path(non_matching_file);
    ft = TEXT;
    const unsigned short res2 = is_matching_path(settings, non_matching_path, &ft, 0, 0);
    color = res2 == 0 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_path(\"%s\"): %d%s\n", color, non_matching_file, res2, CONSOLE_COLOR_RESET);
    assert(res2 == 0);

    destroy_settings(settings);
}

void test_is_matching_path_out_file_types(void) {
    printf("\ntest_is_matching_path_out_file_types()\n");

    FindSettings *settings = default_settings();
    Path *p = new_path(".");
    settings->paths = new_path_node(p);
    const FileType file_type = CODE;
    int *ftint = malloc(sizeof(int));
    *ftint = (int)file_type;
    printf("Adding out-file-type: CODE\n");
    settings->out_file_types = empty_int_node();
    add_int_to_int_node(ftint, settings->out_file_types);

    FileTypes *file_types = new_file_types();
    const error_t err = init_file_types(file_types);
    assert(err == E_OK);

    const char* non_matching_file = "finder.c";
    const Path *non_matching_path = new_path(non_matching_file);
    FileType ft = CODE;
    const unsigned short res1 = is_matching_path(settings, non_matching_path, &ft, 0, 0);
    const char* color = res1 == 0 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_path(\"%s\"): %d%s\n", color, non_matching_file, res1, CONSOLE_COLOR_RESET);
    assert(res1 == 0);

    const char* matching_file = "README.md";
    const Path *matching_path = new_path(matching_file);
    ft = TEXT;
    const unsigned short res2 = is_matching_path(settings, matching_path, &ft, 0, 0);
    color = res2 == 1 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
    printf("%sis_matching_path(\"%s\"): %d%s\n", color, matching_file, res2, CONSOLE_COLOR_RESET);
    assert(res2 == 1);

    destroy_settings(settings);
}

void test_follow_symlinks_default_settings(void) {
    printf("\ntest_follow_symlinks_default_settings()\n");

    char bin_path[MAX_HOMEPATH_LENGTH + 1];
    get_xfind_path(bin_path);
    strcat(bin_path, "/bin");

    FindSettings *settings = default_settings();
    Path *p = new_path(bin_path);
    settings->paths = new_path_node(p);

    FileResults *results = empty_file_results();
    const error_t err = find(settings, results);

    assert(err == E_OK);
    const size_t res_count = file_results_count(results);
    if (res_count > 0) {
        const char* color = res_count < 4 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
        printf("%sres_count: %d%s\n", color, (int)res_count, CONSOLE_COLOR_RESET);
        assert(res_count < 4);
    }

    destroy_file_results(results);
    destroy_settings(settings);
}

void test_follow_symlinks_follow_symlinks(void) {
    printf("\ntest_follow_symlinks_follow_symlinks()\n");

    char bin_path[MAX_HOMEPATH_LENGTH + 1];
    get_xfind_path(bin_path);
    strcat(bin_path, "/bin");

    FindSettings *settings = default_settings();
    Path *p = new_path(bin_path);
    settings->paths = new_path_node(p);
    settings->follow_symlinks = true;

    FileResults *results = empty_file_results();
    const error_t err = find(settings, results);

    assert(err == E_OK);
    const size_t res_count = file_results_count(results);
    if (res_count > 0) {
        const char* color = res_count > 2 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
        printf("%sres_count: %d%s\n", color, (int)res_count, CONSOLE_COLOR_RESET);
        assert(res_count > 2);
    }

    destroy_file_results(results);
    destroy_settings(settings);
}

void test_follow_symlinks_no_follow_symlinks(void) {
    printf("\ntest_follow_symlinks_no_follow_symlinks()\n");

    char bin_path[MAX_HOMEPATH_LENGTH + 1];
    get_xfind_path(bin_path);
    strcat(bin_path, "/bin");

    FindSettings *settings = default_settings();
    Path *p = new_path(bin_path);
    settings->paths = new_path_node(p);
    settings->follow_symlinks = false;

    FileResults *results = empty_file_results();
    const error_t err = find(settings, results);

    assert(err == E_OK);
    const size_t res_count = file_results_count(results);
    if (res_count > 0) {
        const char* color = res_count < 4 ? CONSOLE_COLOR_GREEN : CONSOLE_COLOR_RED;
        printf("%sres_count: %d%s\n", color, (int)res_count, CONSOLE_COLOR_RESET);
        assert(res_count < 4);
    }

    destroy_file_results(results);
    destroy_settings(settings);
}

// int main(int argc, char *argv[])
// {
//     if (argc < 2) {
//         handle_error(E_STARTPATH_NOT_DEFINED);
//         print_usage();
//         return E_STARTPATH_NOT_DEFINED;
//     }

//     FindSettings *settings = default_settings();
//     error_t err = settings_from_args(argc - 1, ++argv, settings);
//     if (err != E_OK) {
//         handle_error(err);
//         print_usage();
//         return (int) err;
//     }

//     if (settings->debug) {
//         print_settings(settings);
//     }

//     if (settings->print_usage) {
//         print_usage();
//     } else if (settings->print_version) {
//         // TODO
//     } else {

//         // this will contain the find results
//         FileResults *results = empty_file_results();

//         err = find(settings, results);
//         if (err == E_OK) {
//             if (settings->print_dirs) {
//                 if (is_null_or_empty_file_results(results)) {
//                     printf("\nMatching directories: 0\n");
//                 } else {
//                     print_dir_results(results);
//                 }
//             }

//             if (settings->print_files) {
//                 if (is_null_or_empty_file_results(results)) {
//                     printf("\nMatching files: 0\n");
//                     if (results != NULL) {
//                         destroy_file_results(results);
//                     }
//                 } else {
//                     print_file_results(results, settings->sort_by, settings->sort_case_insensitive,
//                                        settings->sort_descending);
//                     destroy_file_results(results);
//                 }
//             }
//         } else {
//             handle_error(err);
//             print_usage();
//         }
//     }

//     destroy_settings(settings);

//     return (int) err;
// }
