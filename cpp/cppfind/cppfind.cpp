#include "common.h"
#include "Finder.h"
#include "FileResult.h"
#include "StringUtil.h"
#include "FindException.h"
#include "FindOptions.h"

using namespace cppfind;

std::vector<std::string> get_matching_dirs(std::vector<FileResult*>* fileresults) {
    std::set<std::string> dir_set = {};
    std::vector<std::string> matching_dirs = {};
    for (const auto& f : *fileresults) {
        if (dir_set.find(f->path()) == dir_set.end()) {
            matching_dirs.push_back(f->path());
        }
        dir_set.emplace(f->path());
    }
    return matching_dirs;
}

std::vector<std::string> get_matching_files(std::vector<FileResult*>* fileresults) {
    std::vector<std::string> matching_files = {};
    for (const auto& f : *fileresults) {
        matching_files.push_back(f->string());
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
        auto* settings = options->settings_from_args(argc, argv);

        if (settings->debug()) {
            log(settings->string());
        }

        if (settings->printusage()) {
            options->usage();
        }

        auto* finder = new Finder(settings);

        std::vector<FileResult*> fileresults = finder->find();

        if (settings->listdirs()) {
            std::vector<std::string> dirs = get_matching_dirs(&fileresults);
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
            std::vector<std::string> files = get_matching_files(&fileresults);
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
