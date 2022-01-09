#include <boost/filesystem.hpp>
#include "FileResult.h"

namespace cppfind {
    FileResult::FileResult(std::string& p, std::string& f, FileType ft) {
        std::vector<std::string> containers = {};
        init(containers, p, f, ft);
    }

    FileResult::FileResult(const std::vector<std::string>& cs, const std::string& p, const std::string& f, const FileType ft) {
        init(cs, p, f, ft);
    }

    FileResult::FileResult(const std::vector<std::string>& cs, std::string& p, std::string& f, FileType ft) {
        init(cs, p, f, ft);
    }


    void FileResult::init(const std::vector<std::string>& cs, const std::string& p, const std::string& f,
                          const FileType ft) {
        m_containers = cs;
        m_path = p;
        m_filename = f;
        m_filetype = ft;
    }

    std::string FileResult::path() const {
        return m_path;
    }

    std::string FileResult::filename() const {
        return m_filename;
    }

    FileType FileResult::filetype() {
        return m_filetype;
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
