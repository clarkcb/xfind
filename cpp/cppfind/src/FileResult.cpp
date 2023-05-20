#include <boost/filesystem.hpp>
#include "FileResult.h"

namespace cppfind {
    FileResult::FileResult(std::string& p, std::string& f, FileType ft, uint64_t filesize, long modtime) {
        std::vector<std::string> containers = {};
        init(containers, p, f, ft, filesize, modtime);
    }

    FileResult::FileResult(const std::vector<std::string>& cs, const std::string& p, const std::string& f,
                           const FileType ft, const uint64_t filesize, long modtime) {
        init(cs, p, f, ft, filesize, modtime);
    }

    FileResult::FileResult(const std::vector<std::string>& cs, std::string& p, std::string& f, FileType ft,
                           uint64_t filesize, long modtime) {
        init(cs, p, f, ft, filesize, modtime);
    }


    void FileResult::init(const std::vector<std::string>& cs, const std::string& p, const std::string& f,
                          const FileType ft, const uint64_t filesize, long modtime) {
        m_containers = cs;
        m_path = p;
        m_file_name = f;
        m_file_type = ft;
        m_file_size = filesize;
        m_mod_time = modtime;
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
