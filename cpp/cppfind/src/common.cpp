#include <iostream>

#include "common.h"

namespace cppfind {
    void log_msg(const std::string_view msg) {
        std::cout << msg << std::endl << std::flush;
    }

    void log_error(const std::string_view msg) {
        std::cerr << "ERROR: " << msg << std::endl << std::flush;
    }
}
