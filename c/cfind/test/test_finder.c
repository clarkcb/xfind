#include <stdio.h>

#include "test_finder.h"


void test_validate_settings(void)
{
    printf("\ntest_validate_settings()\n");

    FindSettings *settings = default_settings();
    printf("is_null_or_empty_string_node(settings->paths): %d\n", is_null_or_empty_string_node(settings->paths));
    assert(is_null_or_empty_string_node(settings->paths) == 1);

    error_t err = validate_settings(settings);
    if (err == E_STARTPATH_NOT_DEFINED) {
        printf("validate_settings(settings): E_STARTPATH_NOT_DEFINED\n");
    }
    assert(err == E_STARTPATH_NOT_DEFINED);

    // Add paths
    char* p1 = "./non_existent_file.h";
    printf("Adding path: \"%s\"\n", p1);

    settings->paths = new_string_node(p1);
    assert(is_null_or_empty_string_node(settings->paths) == 0);
    printf("string_node_count(settings->paths): %zu\n", string_node_count(settings->paths));
    assert(string_node_count(settings->paths) == 1);

    char* p2 = "./non_existent_file.c";
    printf("Adding path: \"%s\"\n", p2);

    add_string_to_string_node(p2, settings->paths);
    printf("string_node_count(settings->paths): %zu\n", string_node_count(settings->paths));
    assert(string_node_count(settings->paths) == 2);

    err = validate_settings(settings);
    if (err == E_STARTPATH_NOT_FOUND) {
        printf("validate_settings(settings): E_STARTPATH_NOT_FOUND\n");
    }
    assert(err == E_STARTPATH_NOT_FOUND);

    destroy_settings(settings);
}

void test_is_matching_dir(void) {
    printf("\ntest_is_matching_dir()\n");

    FindSettings *settings = default_settings();
    char* p = ".";
    settings->paths = new_string_node(p);

    // test current dot dir
    char* dot_dir = ".";
    unsigned short res1 = is_matching_dir(dot_dir, settings);
    assert(res1 == 1);

    // test parent dot dir
    char* parent_dir = "..";
    unsigned short res2 = is_matching_dir(parent_dir, settings);
    assert(res2 == 1);

    // test "test" dir
    char* test_dir = "test";
    unsigned short res3 = is_matching_dir(test_dir, settings);
    assert(res3 == 1);

    destroy_settings(settings);
}

void test_is_matching_dir_in_dirpatterns(void) {
    printf("\ntest_is_matching_dir_in_dirpatterns()\n");

    FindSettings *settings = default_settings();
    char* p = ".";
    settings->paths = new_string_node(p);

    // test "test" dir with "test" in_dirpattern
    char* test_dir = "test";
    settings->in_dirpatterns = new_regex_node_from_string("test");
    unsigned short res1 = is_matching_dir(test_dir, settings);
    assert(res1 == 1);

    // test "other" dir with "test" in_dirpattern
    char* other_dir = "other";
    unsigned short res2 = is_matching_dir(other_dir, settings);
    assert(res2 == 0);

    destroy_settings(settings);
}

void test_is_matching_dir_out_dirpatterns(void) {
    printf("\ntest_is_matching_dir_out_dirpatterns()\n");

    FindSettings *settings = default_settings();
    char* p = ".";
    settings->paths = new_string_node(p);

    // test "test" dir with "test" out_dirpattern
    char* test_dir = "test";
    settings->out_dirpatterns = new_regex_node_from_string("test");
    unsigned short res1 = is_matching_dir(test_dir, settings);
    assert(res1 == 0);

    // test "other" dir with "test" out_dirpattern
    char* other_dir = "other";
    unsigned short res2 = is_matching_dir(other_dir, settings);
    assert(res2 == 1);

    destroy_settings(settings);
}

void test_filter_file(void) {
    printf("\ntest_filter_file()\n");

    FindSettings *settings = default_settings();
    char* p = ".";
    settings->paths = new_string_node(p);

    FileTypes *filetypes = new_filetypes();
    error_t err = get_filetypes(filetypes);
    assert(err == E_OK);

    Finder *finder = new_finder(settings, filetypes);

    char* test_dir = ".";
    char* test_file = "test_finder.c";
    FileType ft = CODE;
    struct stat fpstat;
    unsigned short res1 = filter_file(test_dir, test_file, finder, &ft, &fpstat);
    assert(res1 == 1);

    char* hidden_file = ".hidden.c";
    unsigned short res2 = filter_file(test_dir, hidden_file, finder, &ft, &fpstat);
    assert(res2 == 0);

    destroy_finder(finder);
    destroy_settings(settings);
}

void test_is_matching_file_in_extensions(void) {
    printf("\ntest_is_matching_file_in_extensions()\n");

    FindSettings *settings = default_settings();
    char* p = ".";
    settings->paths = new_string_node(p);
    settings->in_extensions = new_string_node("c");

    FileTypes *filetypes = new_filetypes();
    error_t err = get_filetypes(filetypes);
    assert(err == E_OK);

    Finder *finder = new_finder(settings, filetypes);

    char* test_dir = ".";
    char* matching_file = "test_finder.c";
    FileType ft = CODE;
    struct stat fpstat;
    unsigned short res1 = is_matching_file(test_dir, matching_file, finder, &ft, &fpstat);
    assert(res1 == 1);

    char* non_matching_file = "test_finder.h";
    unsigned short res2 = is_matching_file(test_dir, non_matching_file, finder, &ft, &fpstat);
    assert(res2 == 0);

    destroy_finder(finder);
    destroy_settings(settings);
}

void test_is_matching_file_out_extensions(void) {
    printf("\ntest_is_matching_file_out_extensions()\n");

    FindSettings *settings = default_settings();
    char* p = ".";
    settings->paths = new_string_node(p);
    settings->out_extensions = new_string_node("c");

    FileTypes *filetypes = new_filetypes();
    error_t err = get_filetypes(filetypes);
    assert(err == E_OK);

    Finder *finder = new_finder(settings, filetypes);

    char* test_dir = ".";
    char* matching_file = "test_finder.c";
    FileType ft = CODE;
    struct stat fpstat;
    unsigned short res1 = is_matching_file(test_dir, matching_file, finder, &ft, &fpstat);
    assert(res1 == 0);

    char* non_matching_file = "test_finder.h";
    unsigned short res2 = is_matching_file(test_dir, non_matching_file, finder, &ft, &fpstat);
    assert(res2 == 1);

    destroy_finder(finder);
    destroy_settings(settings);
}

void test_is_matching_file_in_filepatterns(void) {
    printf("\ntest_is_matching_file_in_filepatterns()\n");

    FindSettings *settings = default_settings();
    char* p = ".";
    settings->paths = new_string_node(p);
    settings->in_filepatterns = new_regex_node_from_string("test");

    FileTypes *filetypes = new_filetypes();
    error_t err = get_filetypes(filetypes);
    assert(err == E_OK);

    Finder *finder = new_finder(settings, filetypes);

    char* test_dir = ".";
    char* matching_file = "test_finder.c";
    FileType ft = CODE;
    struct stat fpstat;
    unsigned short res1 = is_matching_file(test_dir, matching_file, finder, &ft, &fpstat);
    assert(res1 == 1);

    char* non_matching_file = "finder.c";
    unsigned short res2 = is_matching_file(test_dir, non_matching_file, finder, &ft, &fpstat);
    assert(res2 == 0);

    destroy_finder(finder);
    destroy_settings(settings);
}

void test_is_matching_file_out_filepatterns(void) {
    printf("\ntest_is_matching_file_out_filepatterns()\n");

    FindSettings *settings = default_settings();
    char* p = ".";
    settings->paths = new_string_node(p);
    settings->out_filepatterns = new_regex_node_from_string("test");

    FileTypes *filetypes = new_filetypes();
    error_t err = get_filetypes(filetypes);
    assert(err == E_OK);

    Finder *finder = new_finder(settings, filetypes);

    char* test_dir = ".";
    char* matching_file = "test_finder.c";
    FileType ft = CODE;
    struct stat fpstat;
    unsigned short res1 = is_matching_file(test_dir, matching_file, finder, &ft, &fpstat);
    assert(res1 == 0);

    char* non_matching_file = "finder.c";
    unsigned short res2 = is_matching_file(test_dir, non_matching_file, finder, &ft, &fpstat);
    assert(res2 == 1);

    destroy_finder(finder);
    destroy_settings(settings);
}

void test_is_matching_file_in_filetypes(void) {
    printf("\ntest_is_matching_file_in_filetypes()\n");

    FindSettings *settings = default_settings();
    char* p = ".";
    settings->paths = new_string_node(p);
    FileType filetype = CODE;
    int *ftint = malloc(sizeof(int));
    *ftint = (int)filetype;
    settings->in_filetypes = empty_int_node();
    add_int_to_int_node(ftint, settings->in_filetypes);

    FileTypes *filetypes = new_filetypes();
    error_t err = get_filetypes(filetypes);
    assert(err == E_OK);

    Finder *finder = new_finder(settings, filetypes);

    char* test_dir = ".";
    char* matching_file = "finder.c";
    FileType ft = CODE;
    struct stat fpstat;
    unsigned short res1 = is_matching_file(test_dir, matching_file, finder, &ft, &fpstat);
    assert(res1 == 1);

    char* non_matching_file = "README.md";
    ft = TEXT;
    unsigned short res2 = is_matching_file(test_dir, non_matching_file, finder, &ft, &fpstat);
    assert(res2 == 0);

    destroy_finder(finder);
    destroy_settings(settings);
}

void test_is_matching_file_out_filetypes(void) {
    printf("\ntest_is_matching_file_out_filetypes()\n");

    FindSettings *settings = default_settings();
    char* p = ".";
    settings->paths = new_string_node(p);
    FileType filetype = CODE;
    int *ftint = malloc(sizeof(int));
    *ftint = (int)filetype;
    settings->out_filetypes = empty_int_node();
    add_int_to_int_node(ftint, settings->out_filetypes);

    FileTypes *filetypes = new_filetypes();
    error_t err = get_filetypes(filetypes);
    assert(err == E_OK);

    Finder *finder = new_finder(settings, filetypes);

    char* test_dir = ".";
    char* non_matching_file = "finder.c";
    FileType ft = CODE;
    struct stat fpstat;
    unsigned short res1 = is_matching_file(test_dir, non_matching_file, finder, &ft, &fpstat);
    assert(res1 == 0);

    char* matching_file = "README.md";
    ft = TEXT;
    unsigned short res2 = is_matching_file(test_dir, matching_file, finder, &ft, &fpstat);
    assert(res2 == 1);

    destroy_finder(finder);
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

//     if (settings->printusage) {
//         print_usage();
//     } else if (settings->printversion) {
//         // TODO
//     } else {

//         // this will contain the find results
//         FileResults *results = empty_file_results();

//         err = find(settings, results);
//         if (err == E_OK) {
//             if (settings->listdirs) {
//                 if (is_null_or_empty_file_results(results)) {
//                     printf("\nMatching directories: 0\n");
//                 } else {
//                     print_dir_results(results);
//                 }
//             }

//             if (settings->listfiles) {
//                 if (is_null_or_empty_file_results(results)) {
//                     printf("\nMatching files: 0\n");
//                     if (results != NULL) {
//                         destroy_file_results(results);
//                     }
//                 } else {
//                     print_file_results(results, settings->sortby, settings->sort_caseinsensitive,
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
