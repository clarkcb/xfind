#include "common.h"
#include "FindException.h"
#include "FindOptions.h"
#include "Finder.h"

using namespace cppfind;

std::vector<std::string> get_matching_dirs(std::vector<FileResult>& file_results) {
    std::set<std::string> dir_set{};
    std::vector<std::string> matching_dirs = {};
    for (const auto fr : file_results) {
        if (dir_set.find(fr.path()) == dir_set.end()) {
            matching_dirs.push_back(fr.path());
        }
        dir_set.emplace(fr.path());
    }
    return matching_dirs;
}

std::vector<std::string> get_matching_files(std::vector<FileResult>& file_results) {
    std::vector<std::string> matching_files{};
    for (const auto fr : file_results) {
        matching_files.push_back(fr.string());
    }
    return matching_files;
}

int main(int argc, char *argv[]) {
    FindOptions* options;

    try {
        options = new FindOptions();
    } catch (const FindException& e) {
        log("");
        log_error(e.what());
        exit(1);
    }

    try {
        auto settings = options->settings_from_args(argc, argv);

        if (settings->debug()) {
            log(settings->string());
        }

        if (settings->print_usage()) {
            options->usage();
        }

        auto* finder = new Finder(*settings);

        std::vector<FileResult> file_results = finder->find();

        if (settings->list_dirs()) {
            std::vector<std::string> dirs = get_matching_dirs(file_results);
            std::string msg = "\nMatching directories";
            if (dirs.empty()) {
                msg.append(": 0");
                log(msg);
            } else {
                msg.append(" (").append(std::to_string(dirs.size())).append("):");
                log(msg);
                for (const auto& d : dirs) {
                    log(d);
                }
            }
        }

        if (settings->list_files()) {
            std::vector<std::string> files = get_matching_files(file_results);
            std::string msg = "\nMatching files";
            if (files.empty()) {
                msg.append(": 0");
                log(msg);
            } else {
                msg.append(" (").append(std::to_string(files.size())).append("):");
                log(msg);
                for (const auto& f : files) {
                    log(f);
                }
            }
        }
    } catch (const FindException& e) {
        log("");
        log_error(e.what());
        options->usage();
    }

    return 0;
}
