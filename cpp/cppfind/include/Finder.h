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

    public:
        explicit Finder(FindSettings* settings);
        bool filter_file(const std::string& filepath);
        bool is_archive_find_file(const std::string& filepath);
        bool is_find_dir(const std::string& filepath);
        bool is_find_file(const std::string& filepath, const FileType filetype);
        std::vector<FileResult*> find();
    };
}

#endif //CPPFIND_FINDER_H
