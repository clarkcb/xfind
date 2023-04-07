#ifndef CPPFIND_FILERESULT_H
#define CPPFIND_FILERESULT_H

#include <string>
#include <vector>
#include "FileTypes.h"

namespace cppfind {
    class FileResult {
    private:
        const std::string CONTAINER_SEPARATOR = "!";
        std::vector<std::string> m_containers;
        std::string m_path;
        std::string m_filename;
        FileType m_filetype;
        uint64_t m_filesize;
        long m_modtime;
        void init(const std::vector<std::string>& containers, const std::string& path, const std::string& filename,
                  FileType filetype, uint64_t filesize, long modtime);

    public:
        FileResult(std::string& path, std::string& filename, FileType filetype, uint64_t filesize, long modtime);
        FileResult(const std::vector<std::string>& containers, std::string& path, std::string& filename,
                   FileType filetype, uint64_t filesize, long modtime);
        FileResult(const std::vector<std::string>& containers, const std::string& path, const std::string& filename,
                   FileType filetype, uint64_t filesize, long modtime);
        [[nodiscard]] std::string path() const;
        [[nodiscard]] std::string filename() const;
        [[nodiscard]] FileType filetype() const;
        [[nodiscard]] uint64_t filesize() const;
        [[nodiscard]] long modtime() const;
        [[nodiscard]] const std::string string() const;
    };
}

#endif // CPPFIND_FILERESULT_H
