#include <iostream>

#include "common.h"

namespace cppfind {
    void log(const std::string& msg) {
        std::cout << msg << std::endl << std::flush;
    }

    void log_error(const std::string& msg) {
        std::cerr << "ERROR: " << msg << std::endl << std::flush;
    }
}
