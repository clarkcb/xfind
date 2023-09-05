#ifndef CPPFIND_FINDER_H
#define CPPFIND_FINDER_H

#include <sys/stat.h>
#include "FileResult.h"
#include "FindSettings.h"

namespace cppfind {
    class Finder {
    private:
        FileTypes* m_file_types;
        FindSettings* m_settings;
        static void validate_settings(FindSettings* settings);
        FileResult* get_file_result(std::string& file_path);
        std::vector<FileResult*> get_file_results(const std::string& file_path, const int depth);
        void sort_file_results(std::vector<FileResult*>& file_results);

    public:
        explicit Finder(FindSettings* settings);
        // bool filter_file(const std::string& file_path);
        std::optional<FileResult*> filter_to_file_result(const std::string& file_path);
        bool is_matching_archive_file(const std::string& file_name);
        bool is_matching_dir(const std::string& file_path);
        bool is_matching_file(const std::string& file_name, const FileType file_type, const struct stat*);
        std::vector<FileResult*> find();
    };
}

#endif //CPPFIND_FINDER_H
