#include "common.h"
#include "FileResultFormatter.h"
#include "FindException.h"
#include "FindOptions.h"
#include "Finder.h"

using namespace cppfind;

std::vector<std::filesystem::path> get_matching_dir_paths(const std::vector<FileResult>& file_results) {
    std::unordered_set<std::string> dir_set;
    std::vector<std::filesystem::path> matching_dir_paths;
    for (const auto& fr : file_results) {
        const std::string dir = fr.file_path().parent_path().string();
        if (!dir_set.contains(dir)) {
            matching_dir_paths.push_back(fr.file_path().parent_path());
        }
        dir_set.emplace(dir);
    }
    return matching_dir_paths;
}

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
            const std::vector<std::filesystem::path> dir_paths = get_matching_dir_paths(file_results);
            std::string msg{"\nMatching directories"};
            if (dir_paths.empty()) {
                msg.append(": 0");
                log_msg(msg);
            } else {
                msg.append(" (").append(std::to_string(dir_paths.size())).append("):");
                log_msg(msg);
                for (const auto& d : dir_paths) {
                    log_msg(formatter.format_dir_path(d));
                }
            }
        }

        if (settings.print_files()) {
            std::string msg{"\nMatching files"};
            if (file_results.empty()) {
                msg.append(": 0");
                log_msg(msg);
            } else {
                msg.append(" (").append(std::to_string(file_results.size())).append("):");
                log_msg(msg);
                for (const auto& fr : file_results) {
                    log_msg(formatter.format_file_result(fr));
                }
            }
        }
    } catch (const FindException& e) {
        log_msg("");
        log_error(e.what());
        options->usage();
    }

    return 0;
}
