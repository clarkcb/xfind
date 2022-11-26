#ifndef CPPFIND_FINDER_H
#define CPPFIND_FINDER_H

#include "FileResult.h"
#include "FindSettings.h"

namespace cppfind {
    class Finder {
    private:
        FileTypes* m_filetypes;
        FindSettings* m_settings;
        static void validate_settings(FindSettings* settings);
        FileResult* get_file_result(std::string& filepath);
        std::vector<FileResult*> get_file_results(const std::string& filepath);
        void sort_file_results(std::vector<FileResult*> fileresults);

    public:
        explicit Finder(FindSettings* settings);
        bool filter_file(const std::string& filepath);
        std::optional<FileResult*> filter_to_file_result(const std::string& filepath);
        bool is_matching_archive_file(const std::string& filename);
        bool is_matching_dir(const std::string& filepath);
        bool is_matching_file(const std::string& filename, const FileType filetype);
        std::vector<FileResult*> find();
    };
}

#endif //CPPFIND_FINDER_H
