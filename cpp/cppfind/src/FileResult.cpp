#include "FileResult.h"

namespace cppfind {
    FileResult::FileResult(const std::filesystem::path& file_path, const FileType file_type, const uint64_t file_size,
        const long last_mod) :
    m_file_path{file_path}, m_file_type{file_type}, m_file_size{file_size}, m_last_mod{last_mod} {
        this->m_containers = {};
    }

    FileResult::FileResult(const std::vector<std::filesystem::path>& containers, const std::filesystem::path& file_path,
                           const FileType file_type, const uint64_t file_size, const long last_mod) :
    m_containers{containers}, m_file_path{file_path}, m_file_type{file_type}, m_file_size{file_size},
    m_last_mod{last_mod} {
    }

    std::vector<std::filesystem::path> FileResult::containers() const {
        return m_containers;
    }

    std::filesystem::path FileResult::file_path() const {
        return m_file_path;
    }

    std::string FileResult::file_name() const {
        return m_file_path.filename().string();
    }

    FileType FileResult::file_type() const {
        return m_file_type;
    }

    uint64_t FileResult::file_size() const {
        return m_file_size;
    }

    long FileResult::last_mod() const {
        return m_last_mod;
    }

    std::string FileResult::string() const {
        std::string fullpath{};
        for (const auto& c : m_containers) {
            fullpath.append(c).append(CONTAINER_SEPARATOR);
        }
        fullpath.append(m_file_path.string());
        return fullpath;
    }
}
