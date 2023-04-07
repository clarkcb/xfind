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
        m_filename = f;
        m_filetype = ft;
        m_filesize = filesize;
        m_modtime = modtime;
    }

    std::string FileResult::path() const {
        return m_path;
    }

    std::string FileResult::filename() const {
        return m_filename;
    }

    FileType FileResult::filetype() const {
        return m_filetype;
    }

    uint64_t FileResult::filesize() const {
        return m_filesize;
    }

    long FileResult::modtime() const {
        return m_modtime;
    }

    const std::string FileResult::string() const {
        std::string fullpath;
        for (const auto& c : m_containers) {
            fullpath.append(c).append(CONTAINER_SEPARATOR);
        }
        boost::filesystem::path p(m_path);
        p.append(m_filename);
        fullpath.append(p.string());
        return fullpath;
    }
}
