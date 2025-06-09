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

    if (settings->print_usage) {
        print_usage();
    } else if (settings->print_version) {
        // TODO
    } else {

        // this will contain the find results
        FileResults *results = empty_file_results();

        err = find(settings, results);
        if (err == E_OK) {
            if (settings->print_dirs) {
                print_dir_results(results, settings);
            }

            if (settings->print_files) {
                print_file_results(results, settings);
                if (results != NULL) {
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
