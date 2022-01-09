#include "common.h"
#include "Finder.h"
#include "FileResult.h"
#include "StringUtil.h"
#include "FindException.h"
#include "FindOptions.h"

using namespace cppfind;

std::vector<std::string> get_matching_dirs(std::vector<FileResult*>* findfiles) {
    std::set<std::string> matching_dir_set = {};
    for (const auto& f : *findfiles) {
        matching_dir_set.insert(f->path());
    }
    std::vector<std::string> matching_dirs(matching_dir_set.begin(), matching_dir_set.end());
    return matching_dirs;
}

std::vector<std::string> get_matching_files(std::vector<FileResult*>* findfiles) {
    std::set<std::string> matching_file_set = {};
    for (const auto& f : *findfiles) {
        matching_file_set.insert(f->string());
    }
    std::vector<std::string> matching_files(matching_file_set.begin(), matching_file_set.end());
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
        auto* settings = options->settings_from_args(argc, argv);

        if (settings->debug()) {
            log(settings->string());
        }

        if (settings->printusage()) {
            options->usage();
        }

        auto* finder = new Finder(settings);

        std::vector<FileResult*> findfiles = finder->find();

        if (settings->listdirs()) {
            std::vector<std::string> dirs = get_matching_dirs(&findfiles);
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

        if (settings->listfiles()) {
            std::vector<std::string> files = get_matching_files(&findfiles);
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
