#include <sys/stat.h>
#include <fstream>
#include <sstream>

#include "FileUtil.h"

namespace cppfind {
    // TODO: find lib function for this, current implementation is incomplete
    std::string FileUtil::expand_path(const std::string& file_path) {
        if (file_path.at(0) == '~') {
            std::string expanded = getenv("HOME");
            if (file_path.length() > 1) {
                expanded.append(file_path.substr(1));
            }
            return expanded;
        }
        return file_path;
    }

    bool FileUtil::file_exists(const std::string& file_path) {
        struct stat st;
        return (stat(file_path.c_str(), &st) == 0);
    }

    uint64_t FileUtil::file_size(const std::string& file_path) {
        struct stat st;
        if (stat(file_path.c_str(), &st)) /*failure*/
            return -1; // when file does not exist or is not accessible
        return (uint64_t) st.st_size;
    }

    std::string FileUtil::get_contents(const std::ifstream& fin) {
        std::stringstream sstr;
        sstr << fin.rdbuf();
        return sstr.str();
    }

    // implement the get_extension method
    std::string FileUtil::get_extension(const std::string& name) {
        const size_t pos = name.rfind('.');
        if (pos == 0 || pos == std::string::npos) {
            return "";
        }
        return name.substr(pos + 1);
    }

    // implement the get_file_name method
    // TODO: make this cross-platform
    std::string FileUtil::get_file_name(const std::string& file_path) {
        // TODO: make this cross-platform
        const size_t pos = file_path.rfind('/');
        if (pos == std::string::npos) {
            return file_path;
        }
        return file_path.substr(pos + 1);
    }

    // implement the is_directory method
    bool FileUtil::is_directory(const std::string& name) {
        struct stat st;
        if (stat(name.c_str(), &st)) /*failure*/
            return false; // when file does not exist or is not accessible
        return (st.st_mode & S_IFDIR) != 0;
    }

    // implement the is_regular_file method
    bool FileUtil::is_regular_file(const std::string& name) {
        struct stat st;
        if (stat(name.c_str(), &st)) /*failure*/
            return false; // when file does not exist or is not accessible
        return (st.st_mode & S_IFREG) != 0;
    }

    // implement the is_dot_dir method
    bool FileUtil::is_dot_dir(const std::string& name) {
        return name == "." || name == ".." ||
            name == "./" || name == "../" ||
            name == ".\\" || name == "..\\";
    }

    // implement the is_hidden method
    bool FileUtil::is_hidden(const std::string& name) {
        return !name.empty() && name[0] == '.' && !FileUtil::is_dot_dir(name);
    }

    // implement the join_path method
    // TODO: make this cross-platform
    std::string FileUtil::join_path(const std::string& path1, const std::string& path2) {
        if (path1.empty()) {
            return path2;
        }
        if (path2.empty()) {
            return path1;
        }
        if (path1[path1.length() - 1] == '/' || path1[path1.length() - 1] == '\\') {
            return path1 + path2;
        }
        return path1 + "/" + path2;
    }

    // implement the split_path method
    // TODO: make this cross-platform
    std::pair<std::string, std::string> FileUtil::split_path(const std::string& file_path) {
        std::string fp{file_path};
        size_t pos = fp.rfind('/');
        size_t last_pos = fp.length() - 1;
        if (pos == last_pos) {
            fp = fp.substr(0, last_pos);
            pos = fp.rfind('/');
        }
        if (pos == std::string::npos) {
            return std::make_pair("", fp);
        }
        return std::make_pair(fp.substr(0, pos), fp.substr(pos + 1));
    }
}
