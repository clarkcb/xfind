#include <boost/filesystem.hpp>
#include "FileResult.h"

namespace cppfind {
    FileResult::FileResult(const std::string& path, const std::string& file_name, const FileType file_type,
                           const uint64_t file_size, const long mod_time) {
        std::vector<std::string> containers{};
        init(containers, path, file_name, file_type, file_size, mod_time);
    }

    FileResult::FileResult(const std::vector<std::string>& containers, const std::string& path,
                           const std::string& file_name, const FileType file_type, const uint64_t file_size,
                           const long mod_time) {
        init(containers, path, file_name, file_type, file_size, mod_time);
    }

    void FileResult::init(const std::vector<std::string>& containers, const std::string& path,
                          const std::string& file_name, const FileType file_type, const uint64_t file_size,
                          const long mod_time) {
        m_containers = containers;
        m_path = path;
        m_file_name = file_name;
        m_file_type = file_type;
        m_file_size = file_size;
        m_mod_time = mod_time;
    }

    std::string FileResult::path() const {
        return m_path;
    }

    std::string FileResult::file_name() const {
        return m_file_name;
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

    const std::string FileResult::string() const {
        std::string fullpath;
        for (const auto& c : m_containers) {
            fullpath.append(c).append(CONTAINER_SEPARATOR);
        }
        boost::filesystem::path p(m_path);
        p.append(m_file_name);
        fullpath.append(p.string());
        return fullpath;
    }
}
