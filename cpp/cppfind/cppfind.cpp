#include "common.h"
#include "FileResultFormatter.h"
#include "FindException.h"
#include "FindOptions.h"
#include "Finder.h"

using namespace cppfind;

int main(int argc, char *argv[]) {
    std::unique_ptr<FindOptions> options_ptr;
    std::unique_ptr<FindSettings> settings_ptr;

    try {
        options_ptr = std::make_unique<FindOptions>();
    } catch (const FindException& e) {
        log_msg("");
        log_error(e.what());
        exit(1);
    }

    try {
        const auto settings = options_ptr->settings_from_args(argc, argv);

        if (settings.debug()) {
            log_msg(settings.string());
        }

        if (settings.print_usage()) {
            options_ptr->usage();
        }

        settings_ptr = std::make_unique<FindSettings>(settings);

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
        log_error_color(e.what(), settings_ptr->colorize());
        options_ptr->usage();
    }

    return 0;
}
