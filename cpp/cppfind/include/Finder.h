#ifndef CPPFIND_FINDER_H
#define CPPFIND_FINDER_H

#include "FindFile.h"
#include "FindSettings.h"

namespace cppfind {
    class Finder {
    private:
        FileTypes* m_filetypes;
        FindSettings* m_settings;
        static void validate_settings(FindSettings* settings);
        FindFile* get_findfile(std::string& filepath);
        std::vector<FindFile*> get_find_files(const std::string& filepath);

    public:
        explicit Finder(FindSettings* settings);
        bool filter_file(const std::string& filepath);
        bool is_archive_find_file(const std::string& filepath);
        bool is_find_dir(const std::string& filepath);
        bool is_find_file(const std::string& filepath, const FileType filetype);
        std::vector<FindFile*> find();
    };
}

#endif //CPPFIND_FINDER_H
