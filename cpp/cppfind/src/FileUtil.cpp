#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <sys/stat.h>
#include <fstream>
#include "FileUtil.h"

namespace cppfind {
    // TODO: find lib function for this, current implementation is incomplete
    std::string FileUtil::expand_path(const std::string& filepath) {
        if (filepath.at(0) == '~') {
            std::string expanded = getenv("HOME");
            if (filepath.length() > 1) {
                expanded.append(filepath.substr(1));
            }
            return expanded;
        }
        return filepath;
    }

    bool FileUtil::file_exists(const std::string& filepath) {
        struct stat st;
        return (stat(filepath.c_str(), &st) == 0);
    }

    long FileUtil::file_length(const std::string& filepath) {
        struct stat st;
        if (stat(filepath.c_str(), &st)) /*failure*/
            return -1; // when file does not exist or is not accessible
        return (long) st.st_size;
    }

    std::string FileUtil::get_contents(const std::string& filepath) {
        std::ifstream fin(filepath);
        std::string contents = get_contents(fin);
        fin.close();
        return contents;
    }

    std::string FileUtil::get_contents(const std::ifstream& fin) {
        std::stringstream sstr;
        sstr << fin.rdbuf();
        return sstr.str();
    }

    std::string FileUtil::get_extension(const std::string& name) {
        boost::filesystem::path path(name);
        std::string ext = path.extension().string();
        if (name == ext) {
            return "";
        }
        if (!ext.empty() && ext[0] == '.') {
            ext = ext.substr(1);
        }
        if (ext != "Z") {
            ext = boost::to_lower_copy(ext);
        }
        return ext;
    }

    std::string FileUtil::get_filename(const std::string& filepath) {
        boost::filesystem::path path(filepath);
        std::string filename = path.filename().string();
        return filename;
    }

    bool FileUtil::is_directory(const std::string& name) {
        boost::filesystem::path path(name);
        return boost::filesystem::is_directory(path);
    }

    bool FileUtil::is_regular_file(const std::string& name) {
        boost::filesystem::path path(name);
        return boost::filesystem::is_regular_file(path);
    }

    bool FileUtil::is_dot_dir(const std::string& name) {
        return name == "." || name == ".." ||
            name == "./" || name == "../" ||
            name == ".\\" || name == "..\\";
    }

    bool FileUtil::is_hidden(const std::string& name) {
        boost::filesystem::path path(name);
        return !name.empty() && name.at(0) == '.' && !FileUtil::is_dot_dir(name);
    }

    std::string FileUtil::join_path(const std::string& path1, const std::string& path2) {
        boost::filesystem::path p1(path1);
        boost::filesystem::path p2(path2);
        boost::filesystem::path fullpath = p1 / p2;
        return fullpath.string();
    }

    std::vector<std::string> FileUtil::split_path(const std::string& filepath) {
        std::vector<std::string> parts;
        if (FileUtil::is_dot_dir(filepath)) {
            if (filepath.substr(0, 2) == "..") {
                parts.emplace_back("..");
            } else {
                parts.emplace_back(".");
            }
            return parts;
        }
        int i = 0;
        boost::filesystem::path path(filepath);
        for(auto& part : path) {
            if (part.string() == "." || part.string() == "..") {
                if (i == 0) {
                    parts.push_back(part.string());
                }
            } else if (part.string() != "/") {
                parts.push_back(part.string());
            }
            i++;
        }
        return parts;
    }
}
