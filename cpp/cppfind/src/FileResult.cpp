#include <boost/filesystem.hpp>
#include "FileResult.h"

namespace cppfind {
    FileResult::FileResult(std::filesystem::path&& file_path, const FileType file_type, const uint64_t file_size,
        const long mod_time) : m_file_path(file_path), m_file_type(file_type), m_file_size(file_size), m_mod_time(mod_time) {
        m_containers = {};
    }

    FileResult::FileResult(std::vector<std::filesystem::path>&& containers, std::filesystem::path&& file_path,
                           const FileType file_type, const uint64_t file_size, const long mod_time) :
    m_containers(containers), m_file_path(file_path), m_file_type(file_type), m_file_size(file_size), m_mod_time(mod_time) {
    }

    std::filesystem::path FileResult::file_path() const {
        return m_file_path;
    }

    FileType FileResult::file_type() const {
        return m_file_type;
    }

    uint64_t FileResult::file_size() const {
        return m_file_size;
    }

    long FileResult::mod_time() const {
        return m_mod_time;
    }

    std::string FileResult::string() const {
        std::string fullpath;
        for (const auto& c : m_containers) {
            fullpath.append(c).append(CONTAINER_SEPARATOR);
        }
        fullpath.append(m_file_path.string());
        return fullpath;
    }
}
