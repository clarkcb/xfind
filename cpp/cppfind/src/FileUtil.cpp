#include <sstream>
#include <wordexp.h>

/*
   https://chat.openai.com/c/2e492ac8-5473-4dcb-ac47-5c620804eeff:
   Remember to link against the necessary libraries if you are using this code (e.g., -lshlwapi on Windows).
   Additionally, note that the code uses Windows API (SHGetFolderPath) for Windows and wordexp for Linux,
   and you may need to include the appropriate headers.
*/
// TODO: activate this for Windows tilde expansion
//#if defined(_WIN32)
//#include <Shlobj.h> // For SHGetFolderPath
//#endif

#include "FileUtil.h"

namespace cppfind {
    std::filesystem::path FileUtil::expand_path(const std::filesystem::path& path) {
        std::filesystem::path expanded = path;
        if (path.empty() || path.c_str()[0] != '~') return expanded;

#if defined(_WIN32)
        // TODO: Windows specific: Expand tilde using SHGetFolderPath
        wchar_t userProfilePath[MAX_PATH];
        if (SUCCEEDED(SHGetFolderPath(nullptr, CSIDL_PROFILE, nullptr, 0, userProfilePath))) {
            expandedPath = std::filesystem::path(userProfilePath) / filePath.native().substr(1);
        }
#else
        // Linux and other platforms: Expand tilde using wordexp
        wordexp_t p;
        if (wordexp(path.c_str(), &p, 0) == 0) {
            expanded = std::filesystem::path(p.we_wordv[0]);
            wordfree(&p);
        }
#endif

        return std::filesystem::absolute(expanded);
    }

    bool FileUtil::path_exists(const std::filesystem::path& path) {
        if (!std::filesystem::exists(path)) {
            const std::filesystem::path expanded = expand_path(path);
            return std::filesystem::exists(expanded);
        }
        return true;
    }

    std::string FileUtil::get_path_extension(const std::filesystem::path& file_path) {
        if (file_path.empty() || !file_path.has_extension()) return "";
        const auto ext = file_path.extension();
        if (ext.empty()) {
            return "";
        }
        std::string exts = ext.string();
        if (exts.at(0) == '.') {
            exts = exts.substr(1);
        }
        return exts;
    }

    // Check if name is equals to a "dot dir"
    bool FileUtil::is_dot_dir(const std::string_view file_name) {
        return file_name == "." || file_name == ".." ||
            file_name == "./" || file_name == "../" ||
            file_name == ".\\" || file_name == "..\\";
    }

    bool FileUtil::is_hidden(const std::string_view file_name) {
        return !file_name.empty() && file_name[0] == '.' && !is_dot_dir(file_name);
    }

    bool FileUtil::is_hidden_path(const std::filesystem::path& file_path) {
        if (file_path.empty()) return false;
        for (auto it = file_path.begin(); it != file_path.end(); ++it) {
            if (it->string().at(0) == '.' && !is_dot_dir(it->string())) return true;
        }
        return false;
    }
}
