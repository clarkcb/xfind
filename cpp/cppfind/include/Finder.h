#ifndef CPPFIND_FINDER_H
#define CPPFIND_FINDER_H

#include "FindFile.h"
#include "FindResult.h"
#include "FindSettings.h"

namespace cppfind {
    class Finder {
    private:
        FileTypes* m_filetypes;
        FindSettings* m_settings;
        static void validate_settings(FindSettings* settings);
        std::vector<FindResult*> find_path(const std::string& filepath);
        std::vector<FindResult*> find_file(FindFile* sf);
        FindFile* get_findfile(std::string& filepath);
        std::vector<FindFile*> get_find_files(const std::string& filepath);

        std::vector<FindResult*> find_text_file(FindFile* sf);
        std::vector<FindResult*> find_ifstream(std::ifstream& fin);
        std::vector<FindResult*> find_ifstream_lines(std::ifstream& fin);
        std::vector<FindResult*> find_ifstream_contents(std::ifstream& fin);
        std::vector<FindResult*> find_multiline_string(std::string& s);
        std::vector<FindResult*> find_binary_file(FindFile* sf);
        std::vector<FindResult*> find_archive_file(FindFile* sf);

    public:
        explicit Finder(FindSettings* settings);
        bool filter_file(std::string filepath);
        bool is_archive_find_file(const std::string& filepath);
        bool is_find_dir(const std::string& filepath);
        bool is_find_file(const std::string& filepath);
        std::vector<FindResult*> find();
    };
}

#endif //CPPFIND_FINDER_H
