#include "common.h"
#include "FindException.h"
#include "FindOptions.h"
#include "Finder.h"

using namespace cppfind;

std::vector<std::string> get_matching_dirs(const std::vector<FileResult>& file_results) {
    std::unordered_set<std::string> dir_set;
    std::vector<std::string> matching_dirs;
    for (const auto& fr : file_results) {
        const std::string dir = fr.file_path().parent_path().string();
        if (!dir_set.contains(dir)) {
            matching_dirs.push_back(dir);
        }
        dir_set.emplace(dir);
    }
    return matching_dirs;
}

std::vector<std::string> get_matching_files(const std::vector<FileResult>& file_results) {
    std::vector<std::string> matching_files;
    matching_files.reserve(file_results.size());
    for (const auto& fr : file_results) {
        matching_files.push_back(fr.string());
    }
    return matching_files;
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

        const std::unique_ptr<FindSettings> settings_ptr = std::make_unique<FindSettings>(settings);

        const auto finder = Finder(settings_ptr);

        const std::vector<FileResult> file_results = finder.find();

        if (settings.print_dirs()) {
            const std::vector<std::string> dirs = get_matching_dirs(file_results);
            std::string msg{"\nMatching directories"};
            if (dirs.empty()) {
                msg.append(": 0");
                log_msg(msg);
            } else {
                msg.append(" (").append(std::to_string(dirs.size())).append("):");
                log_msg(msg);
                for (const auto& d : dirs) {
                    log_msg(d);
                }
            }
        }

        if (settings.print_files()) {
            const std::vector<std::string> files = get_matching_files(file_results);
            std::string msg{"\nMatching files"};
            if (files.empty()) {
                msg.append(": 0");
                log_msg(msg);
            } else {
                msg.append(" (").append(std::to_string(files.size())).append("):");
                log_msg(msg);
                for (const auto& f : files) {
                    log_msg(f);
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
