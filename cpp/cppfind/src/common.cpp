#include <iostream>

#include "common.h"

#include "color.h"

namespace cppfind {
    void log_msg(const std::string_view msg) {
        std::cout << msg << std::endl << std::flush;
    }

    void log_error(const std::string_view msg) {
        // std::cerr << "ERROR: " << msg << std::endl << std::flush;
        log_error_color(msg, true);
    }

    void log_error_color(const std::string_view msg, const bool colorize) {
        if (colorize) {
            std::string colorized;
            colorized.reserve(msg.length() + 20);
            colorized.append(BOLD_RED);
            colorized.append("ERROR: ");
            colorized.append(msg);
            colorized.append(COLOR_RESET);
            std::cerr << colorized << std::endl << std::flush;
        } else {
            std::cerr << "ERROR: " << msg << std::endl << std::flush;
        }
    }
}
