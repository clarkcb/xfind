#ifndef CPPFIND_FILEUTIL_H
#define CPPFIND_FILEUTIL_H

#include <string>
#include <utility>

namespace cppfind {
    class FileUtil {
    public:
        static std::string expand_path(const std::string& file_path);
        static bool file_exists(const std::string& file_path);
        static uint64_t file_size(const std::string& file_path);
        static std::string get_contents(const std::ifstream& fin);
        static std::string get_extension(const std::string& name);
        static std::string get_file_name(const std::string& file_path);
        static bool is_directory(const std::string& name);
        static bool is_regular_file(const std::string& name);
        static bool is_dot_dir(const std::string& name);
        static bool is_hidden(const std::string& name);
        static std::string join_path(const std::string& path1, const std::string& path2);
        static std::pair<std::string, std::string> split_path(const std::string& file_path);

    private:
        // Disallow creating an instance of this object
        FileUtil() = delete;
    };
}

#endif // CPPFIND_FILEUTIL_H
