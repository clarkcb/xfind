#ifndef CPPFIND_FILERESULT_H
#define CPPFIND_FILERESULT_H

#include <string>
#include <vector>
#include "FileTypes.h"

#define CONTAINER_SEPARATOR "!"

namespace cppfind {
    class FileResult {
    public:
        FileResult(std::string_view path, std::string_view file_name, FileType file_type, uint64_t file_size,
                   long mod_time);
        FileResult(const std::vector<std::string>& containers, std::string_view path, std::string_view file_name,
                   FileType file_type, uint64_t file_size, long mod_time);
        [[nodiscard]] std::string path() const;
        [[nodiscard]] std::string file_name() const;
        [[nodiscard]] FileType file_type() const;
        [[nodiscard]] uint64_t file_size() const;
        [[nodiscard]] long mod_time() const;
        [[nodiscard]] std::string string() const;

    private:
        std::vector<std::string> m_containers;
        std::string m_path;
        std::string m_file_name;
        FileType m_file_type;
        uint64_t m_file_size;
        long m_mod_time;
        void init(const std::vector<std::string>& containers, std::string_view path, std::string_view file_name,
                  FileType file_type, uint64_t file_size, long mod_time);
    };
}

#endif // CPPFIND_FILERESULT_H
