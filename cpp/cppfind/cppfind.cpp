#include "common.h"
#include "Finder.h"
#include "StringUtil.h"
#include "FindException.h"
#include "FindResultFormatter.h"
#include "FindOptions.h"

using namespace cppfind;

std::vector<std::string> get_result_dirs(std::vector<FindResult*>* results) {
    std::set<std::string> result_dir_set = {};
    for (const auto& result : *results) {
        result_dir_set.insert(result->findfile()->path());
    }
    std::vector<std::string> result_dirs(result_dir_set.begin(), result_dir_set.end());
    return result_dirs;
}

std::vector<std::string> get_result_files(std::vector<FindResult*>* results) {
    std::set<std::string> result_file_set = {};
    for (const auto& result : *results) {
        result_file_set.insert(result->findfile()->string());
    }
    std::vector<std::string> result_files(result_file_set.begin(), result_file_set.end());
    return result_files;
}

std::vector<std::string> get_result_lines(std::vector<FindResult*>* results, bool unique) {
    std::set<std::string> result_line_set = {};
    for (const auto& result : *results) {
        result_line_set.insert(result->line());
    }
    std::vector<std::string> result_lines(result_line_set.begin(), result_line_set.end());
    sort(result_lines.begin(), result_lines.end());
    return result_lines;
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

        std::vector<FindResult*> results = finder->find();

        if (settings->printresults()) {
            auto* formatter = new FindResultFormatter(settings);
            std::string msg = "\nFind results (";
            msg.append(std::to_string(results.size())).append("):");
            log(msg);
            for (const auto& result : results) {
                log(formatter->format(result));
            }
        }

        if (settings->listdirs()) {
            std::vector<std::string> result_dirs = get_result_dirs(&results);
            std::string msg = "\nDirectories with matches";
            if (result_dirs.empty()) {
                msg.append(": 0");
                log(msg);
            } else {
                msg.append(" (").append(std::to_string(result_dirs.size())).append("):");
                log(msg);
                for (const auto& d : result_dirs) {
                    log(d);
                }
            }
        }

        if (settings->listfiles()) {
            std::vector<std::string> result_files = get_result_files(&results);
            std::string msg = "\nFiles with matches";
            if (result_files.empty()) {
                msg.append(": 0");
                log(msg);
            } else {
                msg.append(" (").append(std::to_string(result_files.size())).append("):");
                log(msg);
                for (const auto& f : result_files) {
                    log(f);
                }
            }
        }

        if (settings->listlines()) {
            std::vector<std::string> result_lines = get_result_lines(&results, settings->uniquelines());
            std::string msg;
            if (settings->uniquelines()) {
                msg = "\nUnique lines with matches";
            } else {
                msg = "\nLines with matches";
            }
            if (result_lines.empty()) {
                msg.append(": 0");
                log(msg);
            } else {
                msg.append(" (").append(std::to_string(result_lines.size())).append("):");
                log(msg);
                for (const auto& l : result_lines) {
                    log(l);
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
