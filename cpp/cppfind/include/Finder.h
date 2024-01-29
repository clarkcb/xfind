#ifndef CPPFIND_FINDER_H
#define CPPFIND_FINDER_H

#include <sys/stat.h>
#include "FileResult.h"
#include "FindSettings.h"

namespace cppfind {
    class Finder {
    public:
        explicit Finder(const FindSettings& settings);
        // bool filter_file(const std::string& file_path);
        std::optional<FileResult> filter_to_file_result(std::string_view file_path);
        bool is_matching_archive_file(std::string_view file_name);
        bool is_matching_dir(std::string_view file_path);
        bool is_matching_file(std::string_view file_name, const FileType& file_type, const struct stat*);
        bool is_matching_file_type(const FileType& file_type);
        bool is_matching_file_result(const FileResult& file_result);
        std::vector<FileResult> find();

    private:
        FileTypes m_file_types;
        FindSettings m_settings;
        static void validate_settings(const FindSettings& settings);
        FileResult* get_file_result(const std::string& file_path);
        std::vector<FileResult> get_file_results(const std::string& file_path, int depth);
        void sort_file_results(std::vector<FileResult>& file_results);
    };
}

#endif // CPPFIND_FINDER_H
