#ifndef CPPFIND_FILERESULT_H
#define CPPFIND_FILERESULT_H

#include <filesystem>
#include <string>
#include <vector>
#include "FileTypes.h"

#define CONTAINER_SEPARATOR "!"

namespace cppfind {
    class FileResult {
    public:
        FileResult(const std::filesystem::path& file_path, FileType file_type, uint64_t file_size,
                   long last_mod);
        FileResult(const std::vector<std::filesystem::path>& containers, const std::filesystem::path& file_path,
                   FileType file_type, uint64_t file_size, long last_mod);
        [[nodiscard]] std::vector<std::filesystem::path> containers() const;
        [[nodiscard]] std::filesystem::path file_path() const;
        [[nodiscard]] std::string file_name() const;
        [[nodiscard]] FileType file_type() const;
        [[nodiscard]] uint64_t file_size() const;
        [[nodiscard]] long last_mod() const;
        [[nodiscard]] std::string string() const;

    private:
        std::vector<std::filesystem::path> m_containers;
        std::filesystem::path m_file_path;
        FileType m_file_type;
        uint64_t m_file_size;
        long m_last_mod;
    };
}

#endif // CPPFIND_FILERESULT_H
