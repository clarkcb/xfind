#include <stdio.h>
#include <string.h>

#include "common.h"
#include "fileresults.h"
#include "finderr.h"
#include "findsettings.h"
#include "findoptions.h"
#include "finder.h"


#define MAX_USERID_LENGTH 32

int main(int argc, char *argv[])
{
    FindSettings *settings = default_settings();
    int err = settings_from_args(argc, argv, settings);
    if (err != E_OK) {
        handle_error(err);
        print_usage();
        return err;
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

            if (is_null_or_empty_file_results(results)) {
                printf("\nMatching files: 0\n");
            } else {
                print_file_results(results);
            }
        } else {
            handle_error(err);
            print_usage();
        }
    }

    destroy_settings(settings);

    return err;
}
