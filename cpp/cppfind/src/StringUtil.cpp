#include <ctime>

#include "StringUtil.h"

namespace cppfind {
    std::vector<std::string> StringUtil::split_string(const std::string& s, const std::string& delims, const bool exclude_empty) {
        std::vector<std::string> parts;
        size_t start = 0;
        size_t end = 0;
        while (end != std::string::npos) {
            end = s.find_first_of(delims, start);
            std::string part = s.substr(start, end - start);
            if (!part.empty() || !exclude_empty) {
                parts.push_back(part);
            }
            start = end + 1;
        }
        return parts;
    }

    std::vector<std::string> StringUtil::split_string(const std::string& s, const std::string& delims) {
        return split_string(s, delims, true);
    }

    void StringUtil::ltrim(std::string& s) {
        s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
            return !std::isspace(ch);
        }));
    }

    void StringUtil::rtrim(std::string& s) {
        s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
            return !std::isspace(ch);
        }).base(), s.end());
    }

    void StringUtil::trim(std::string &s) {
        ltrim(s);
        rtrim(s);
    }

    bool StringUtil::char_in_string(const char c, const std::string& s) {
        return s.find(c) != std::string::npos;
    }
    bool StringUtil::string_in_set(const std::string& s, const std::set<std::string>& set) {
        return set.find(s) != set.end();
    }

    bool StringUtil::string_in_vector(const std::string& s, const std::vector<std::string>& vec) {
        return std::find(vec.begin(), vec.end(), s) != vec.end();
    }

    std::set<std::string> StringUtil::filter_string_set(const std::set<std::string>& set,
                                                        const std::function<bool(const std::string&)>& predicate) {
        std::set<std::string> filtered = set;
        std::erase_if(filtered, std::not_fn(predicate));
        return filtered;
    }

    std::vector<std::string> StringUtil::filter_string_vector(const std::vector<std::string>& vec,
                                                              const std::function<bool(const std::string&)>& predicate) {
        std::vector<std::string> filtered = vec;
        std::erase_if(filtered, std::not_fn(predicate));
        return filtered;
    }

    std::string StringUtil::bool_to_string(bool b) {
        return b ? "true" : "false";
    }

    // TODO: want string to be sorted
    std::string StringUtil::string_set_to_string(std::set<std::string>& set) {
        std::string ss_string = "[";
        for (auto it = set.begin(); it != set.end(); ++it) {
            ss_string.append("\"");
            ss_string.append(*it);
            ss_string.append("\"");
            if (std::next(it) != set.end()) {
                ss_string.append(", ");
            }
        }
        ss_string.append("]");
        return ss_string;
    }

    long StringUtil::date_str_to_long(const std::string& date_str) {
        std::tm tm{};
        if (strptime(date_str.c_str(), "%Y-%m-%d", &tm) == nullptr) {
            return -1;
        }
        return std::mktime(&tm);
    }

    std::string StringUtil::long_to_date_str(const long t) {
        if (t == 0L) return "0";
        char buf[11];
        struct tm *tm = localtime(&t);

        if (strftime(buf, sizeof(buf), "%Y-%m-%d", tm) == 0) {
            return "0";
        } else {
            std::string ds = "\"";
            ds.append(buf);
            ds.append("\"");
            return ds;
        }
    }
}
