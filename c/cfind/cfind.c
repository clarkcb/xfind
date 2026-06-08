#include <stdio.h>
#include <string.h>

#include "common.h"
#include "fileresults.h"
#include "findoptions.h"
#include "finder.h"

void main_handle_error(const error_t err, const FindSettings *settings)
{
    char err_msg[100];
    err_msg[0] = '\0';
    get_error_message(err, err_msg);
    if (settings->colorize) {
        log_err_color(err_msg);
    } else {
        log_err(err_msg);
    }
}

int main(const int argc, char *argv[])
{
    FindOptions *options = empty_find_options();
    error_t err = get_find_options(options);
    if (err) {
        handle_error(err);
        destroy_find_options(options);
        exit(EXIT_FAILURE);
    }

    FindSettings *settings = get_default_settings();
    err = settings_from_args(argc - 1, ++argv, options, settings);
    if (err != E_OK) {
        main_handle_error(err, settings);
        print_usage();
        return (int) err;
    }

    if (settings->debug) {
        print_settings(settings);
    }

    Finder *finder = new_finder(settings);

    // this will contain the find results
    FileResults *results = empty_file_results();

    if (settings->print_usage) {
        print_usage();
    } else if (settings->print_version) {
        // TODO
    } else {

        err = find(finder, results);
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
            main_handle_error(err, settings);
            print_usage();
        }
    }

    destroy_file_results(results);
    destroy_finder(finder);
    destroy_settings(settings);

    return (int) err;
}
