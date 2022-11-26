#include <stdio.h>

#include "fileresults.h"
#include "findoptions.h"
#include "finder.h"


int main(int argc, char *argv[])
{
    if (argc < 2) {
        handle_error(E_STARTPATH_NOT_DEFINED);
        print_usage();
        return E_STARTPATH_NOT_DEFINED;
    }

    FindSettings *settings = default_settings();
    error_t err = settings_from_args(argc - 1, ++argv, settings);
    if (err != E_OK) {
        handle_error(err);
        print_usage();
        return (int) err;
    }

    if (settings->debug) {
        print_settings(settings);
    }

    if (settings->printusage) {
        print_usage();
    } else if (settings->printversion) {
        // TODO
    } else {

        // this will contain the find results
        FileResults *results = empty_file_results();

        err = find(settings, results);
        if (err == E_OK) {
            if (settings->listdirs) {
                if (is_null_or_empty_file_results(results)) {
                    printf("\nMatching directories: 0\n");
                } else {
                    print_dir_results(results);
                }
            }

            if (settings->listfiles) {
                if (is_null_or_empty_file_results(results)) {
                    printf("\nMatching files: 0\n");
                    if (results != NULL) {
                        destroy_file_results(results);
                    }
                } else {
                    print_file_results(results, settings->sortby, settings->sort_descending);
                    destroy_file_results(results);
                }
            }
        } else {
            handle_error(err);
            print_usage();
        }
    }

    destroy_settings(settings);

    return (int) err;
}
