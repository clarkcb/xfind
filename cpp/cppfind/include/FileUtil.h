#ifndef CPPFIND_FILEUTIL_H
#define CPPFIND_FILEUTIL_H

#include <filesystem>
#include <string>

namespace cppfind {
    class FileUtil {
    public:
        static std::filesystem::path expand_tilde(const std::filesystem::path& path);
        static bool path_exists(const std::filesystem::path& path);
        static std::string get_path_extension(const std::filesystem::path& file_path);
        static bool is_dot_dir(std::string_view file_name);
        static bool is_hidden(std::string_view file_name);
        static bool is_hidden_path(const std::filesystem::path& file_path);

    private:
        // Disallow creating an instance of this object
        FileUtil() = delete;
    };
}

#endif // CPPFIND_FILEUTIL_H
