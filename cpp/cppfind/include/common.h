#ifndef CPPFIND_COMMON_H
#define CPPFIND_COMMON_H

#include <string>

namespace cppfind {
    void log_msg(std::string_view msg);
    void log_error(std::string_view msg);
}

#endif // CPPFIND_COMMON_H
