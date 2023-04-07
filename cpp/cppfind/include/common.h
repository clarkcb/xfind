#ifndef CPPFIND_COMMON_H
#define CPPFIND_COMMON_H

#include <string>

namespace cppfind {
    long datestr_to_long(const std::string& datestr);
    std::string long_to_datestr(const long time);
    void log(const std::string& name);
    void log_error(const std::string& name);
}

#endif //CPPFIND_COMMON_H
