#ifndef CPPFIND_FILETYPES_H
#define CPPFIND_FILETYPES_H

#include <set>

namespace cppfind {
    enum class FileType {UNKNOWN, ARCHIVE, BINARY, CODE, TEXT, XML};

    class FileTypes {
    public:
        FileTypes();
        ~FileTypes();
        static FileType from_name(const std::string& name);
        static std::string to_name(const FileType& filetype);
        FileType get_file_type(const std::string& file_path);
        bool is_archive_file(const std::string& file_path);
        bool is_binary_file(const std::string& file_path);
        bool is_code_file(const std::string& file_path);
        bool is_text_file(const std::string& file_path);
        bool is_unknown_file(const std::string& file_path);
        bool is_xml_file(const std::string& file_path);
    private:
        std::set<std::string> m_archive_extensions;
        std::set<std::string> m_archive_names;
        std::set<std::string> m_binary_extensions;
        std::set<std::string> m_binary_names;
        std::set<std::string> m_code_extensions;
        std::set<std::string> m_code_names;
        std::set<std::string> m_text_extensions;
        std::set<std::string> m_text_names;
        std::set<std::string> m_xml_extensions;
        std::set<std::string> m_xml_names;
        void load_file_types();
    };
}

#endif // CPPFIND_FILETYPES_H
