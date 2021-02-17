#include <boost/filesystem.hpp>
#include "FindFile.h"

namespace cppfind {
    FindFile::FindFile(std::string& p, std::string& f, FileType ft) {
        std::vector<std::string> containers = {};
        init(containers, p, f, ft);
    }

    FindFile::FindFile(const std::vector<std::string>& cs, const std::string& p, const std::string& f, const FileType ft) {
        init(cs, p, f, ft);
    }

    FindFile::FindFile(const std::vector<std::string>& cs, std::string& p, std::string& f, FileType ft) {
        init(cs, p, f, ft);
    }


    void FindFile::init(const std::vector<std::string>& cs, const std::string& p, const std::string& f,
                          const FileType ft) {
        m_containers = cs;
        m_path = p;
        m_filename = f;
        m_filetype = ft;
    }

    std::string FindFile::path() const {
        return m_path;
    }

    std::string FindFile::filename() const {
        return m_filename;
    }

    FileType FindFile::filetype() {
        return m_filetype;
    }

    const std::string FindFile::string() const {
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
