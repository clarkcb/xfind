#include "config.h"

namespace cppfind {
    std::string xfindpath() {
        std::string xfindpath = std::getenv("XFIND_PATH");
        if (xfindpath.empty()) {
            const std::string home = std::getenv("HOME");
            if (home.empty()) {
                // TODO: throw exception?
                return "";
            }
            // TODO: make this cross-platform
            return home + "/src/xfind";
        }
        return xfindpath;
    }
}
