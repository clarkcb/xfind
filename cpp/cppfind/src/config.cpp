#include <cstdlib>
#include <boost/filesystem.hpp>
#include "config.h"

namespace cppfind {
    std::string xfindpath() {
        const char* x_p = std::getenv("XFIND_PATH");
        if (x_p == nullptr) {
            const char* h_p = std::getenv("HOME");
            if (h_p == nullptr) {
                return "";
            }
            boost::filesystem::path home(h_p);
            boost::filesystem::path path("src/xfind");
            boost::filesystem::path fullpath = home / path;
            return fullpath.string();
        }
        return {x_p};
    }
}
