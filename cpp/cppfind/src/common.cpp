#include <iostream>
#include <ctime>
#include "common.h"

namespace cppfind {
    long datestr_to_long(const std::string& datestr) {
        std::tm tm = {};
        if (strptime(datestr.c_str(), "%Y-%m-%d", &tm) == nullptr) {
            return -1;
        }
        return std::mktime(&tm);
    }

    std::string long_to_datestr(const long t) {
        if (t == 0L) return "";
        char buf[11];
        struct tm *tm = localtime(&t);

        if (strftime(buf, sizeof(buf), "%Y-%m-%d", tm) == 0) {
            return "0";
        } else {
            return {buf};
        }
    }

    void log(const std::string& msg) {
        std::cout << msg << std::endl;
    }

    void log_error(const std::string& msg) {
        std::cout << "ERROR: " << msg << std::endl;
    }
}
