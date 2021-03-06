#ifndef CPPFIND_FINDFILE_H
#define CPPFIND_FINDFILE_H

#include <string>
#include <vector>
#include "FileTypes.h"

namespace cppfind {
    class FindFile {
    private:
        const std::string CONTAINER_SEPARATOR = "!";
        std::vector<std::string> m_containers;
        std::string m_path;
        std::string m_filename;
        FileType m_filetype;
        void init(const std::vector<std::string>& containers, const std::string& path, const std::string& filename, FileType filetype);

    public:
        FindFile(std::string& path, std::string& filename, FileType filetype);
        FindFile(const std::vector<std::string>& containers, std::string& path, std::string& filename, FileType filetype);
        FindFile(const std::vector<std::string> &containers, const std::string& path, const std::string& filename, FileType filetype);
        std::string path() const;
        std::string filename() const;
        FileType filetype();
        const std::string string() const;
    };
}

#endif // CPPFIND_FINDFILE_H
