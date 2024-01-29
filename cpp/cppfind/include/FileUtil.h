#ifndef CPPFIND_FILEUTIL_H
#define CPPFIND_FILEUTIL_H

#include <string>
#include <utility>

namespace cppfind {
    class FileUtil {
    public:
        static std::string expand_path(std::string_view file_path);
        static bool file_exists(std::string_view file_path);
        static uint64_t file_size(std::string_view file_path);
        static std::string get_contents(const std::ifstream& fin);
        static std::string get_extension(std::string_view name);
        static std::string get_file_name(std::string_view file_path);
        static bool is_directory(std::string_view name);
        static bool is_regular_file(std::string_view name);
        static bool is_dot_dir(std::string_view name);
        static bool is_hidden(std::string_view name);
        static std::string join_path(std::string_view path1, std::string_view path2);
        static std::pair<std::string, std::string> split_path(std::string_view file_path);

    private:
        // Disallow creating an instance of this object
        FileUtil() = delete;
    };
}

#endif // CPPFIND_FILEUTIL_H
