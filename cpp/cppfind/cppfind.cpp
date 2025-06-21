#include "common.h"
#include "FileResultFormatter.h"
#include "FindException.h"
#include "FindOptions.h"
#include "Finder.h"

using namespace cppfind;

int main(int argc, char *argv[]) {
    std::unique_ptr<FindOptions> options;

    try {
        options = std::make_unique<FindOptions>();
    } catch (const FindException& e) {
        log_msg("");
        log_error(e.what());
        exit(1);
    }

    try {
        const auto settings = options->settings_from_args(argc, argv);

        if (settings.debug()) {
            log_msg(settings.string());
        }

        if (settings.print_usage()) {
            options->usage();
        }

        const auto settings_ptr = std::make_unique<FindSettings>(settings);

        const auto finder = Finder(settings_ptr);

        const std::vector<FileResult> file_results = finder.find();

        const auto formatter = FileResultFormatter(settings_ptr);

        if (settings.print_dirs()) {
            print_file_result_dirs(file_results, formatter);
        }

        if (settings.print_files()) {
            print_file_results(file_results, formatter);
        }
    } catch (const FindException& e) {
        log_msg("");
        log_error(e.what());
        options->usage();
    }

    return 0;
}
