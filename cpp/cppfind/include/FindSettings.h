#ifndef CPPFIND_FINDSETTINGS_H
#define CPPFIND_FINDSETTINGS_H

#include <cstdlib>
#include "FileTypes.h"
#include "FindPattern.h"

namespace cppfind {
    enum class SortBy {FILEPATH, FILENAME, FILETYPE};

    class FindSettings {
    private:
        bool m_archivesonly = false;
        bool m_debug = false;
        bool m_excludehidden = true;

        std::vector<std::string> m_in_archiveextensions;
        std::vector<FindPattern*> m_in_archivefilepatterns;
        std::vector<FindPattern*> m_in_dirpatterns;
        std::vector<std::string> m_in_extensions;
        std::vector<FindPattern*> m_in_filepatterns;
        std::vector<FileType> m_in_filetypes;

        bool m_includearchives = false;
        bool m_listdirs = false;
        bool m_listfiles = false;

        std::vector<std::string> m_out_archiveextensions;
        std::vector<FindPattern*> m_out_archivefilepatterns;
        std::vector<FindPattern*> m_out_dirpatterns;
        std::vector<std::string> m_out_extensions;
        std::vector<FindPattern*> m_out_filepatterns;
        std::vector<FileType> m_out_filetypes;

        bool m_printusage = false;
        bool m_printversion = false;
        bool m_recursive = true;

        std::vector<std::string> m_paths;

        SortBy m_sortby = SortBy::FILEPATH;
        bool m_sort_descending = true;
        bool m_verbose = false;

        static std::string bool_to_string(bool b);
        static std::string string_vector_to_string(std::vector<std::string>* s);
        static std::string findpatterns_to_string(std::vector<FindPattern*>* ps);
        static std::string filetypes_to_string(std::vector<FileType>* ts);

        static void add_pattern(const std::string& p, std::vector<FindPattern*>* ps);
        static void add_extensions(const std::string& exts, std::vector<std::string>* extensions);

    public:
        FindSettings();
        void add_in_archiveextension(const std::string& ext);
        void add_in_archivefilepattern(const std::string& pattern);
        void add_in_dirpattern(const std::string& pattern);
        void add_in_extension(const std::string& ext);
        void add_in_filepattern(const std::string& pattern);
        void add_in_filetype(FileType filetype);
        void add_in_linesafterpattern(const std::string& pattern);
        void add_in_linesbeforepattern(const std::string& pattern);
        void add_linesaftertopattern(const std::string& pattern);
        void add_linesafteruntilpattern(const std::string& pattern);
        void add_out_archiveextension(const std::string& ext);
        void add_out_archivefilepattern(const std::string& pattern);
        void add_out_dirpattern(const std::string& pattern);
        void add_out_extension(const std::string& ext);
        void add_out_filepattern(const std::string& pattern);
        void add_out_filetype(FileType filetype);
        void add_out_linesafterpattern(const std::string& pattern);
        void add_out_linesbeforepattern(const std::string& pattern);
        void add_path(const std::string& path);

        bool archivesonly() const;
        bool debug() const;
        bool excludehidden() const;
        bool includearchives() const;
        bool listdirs() const;
        bool listfiles() const;
        bool printusage() const;
        bool printversion() const;
        bool recursive() const;
        bool sort_descending() const;
        bool uniquelines() const;
        bool verbose() const;

        std::vector<std::string>* in_archiveextensions();
        std::vector<FindPattern*>* in_archivefilepatterns();
        std::vector<FindPattern*>* in_dirpatterns();
        std::vector<std::string>* in_extensions();
        std::vector<FindPattern*>* in_filepatterns();
        std::vector<FileType>* in_filetypes();
        std::vector<FindPattern*>* in_linesafterpatterns();
        std::vector<FindPattern*>* in_linesbeforepatterns();

        std::vector<std::string>* out_archiveextensions();
        std::vector<FindPattern*>* out_archivefilepatterns();
        std::vector<FindPattern*>* out_dirpatterns();
        std::vector<std::string>* out_extensions();
        std::vector<FindPattern*>* out_filepatterns();
        std::vector<FileType>* out_filetypes();
        std::vector<FindPattern*>* out_linesafterpatterns();
        std::vector<FindPattern*>* out_linesbeforepatterns();

        std::vector<std::string>* paths();

        SortBy sortby();

        // bool is_in_archiveextension(const std::string* ext);
        // bool is_in_extension(const std::string* ext);
        // bool is_in_filetype(const FileType* m_filetype);
        // bool is_out_archiveextension(const std::string* ext);
        // bool is_out_extension(const std::string* ext);

        void archivesonly(bool b);
        void debug(bool b);
        void excludehidden(bool b);
        void includearchives(bool b);
        void listdirs(bool b);
        void listfiles(bool b);
        void printusage(bool b);
        void printversion(bool b);
        void recursive(bool b);
        void sortby(SortBy sortby);
        void set_sortby(const std::string& name);
        void sort_descending(bool b);
        void verbose(bool b);
        std::string string();

        static SortBy sortby_from_name(const std::string& name);
        static std::string sortby_to_name(const SortBy sortby);
    };
}

#endif //CPPFIND_FINDSETTINGS_H
