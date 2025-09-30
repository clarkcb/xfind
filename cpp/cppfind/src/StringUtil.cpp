#include "StringUtil.h"

namespace cppfind {
    std::vector<std::string> StringUtil::split_string(const std::string_view s, const std::string_view delims,
                                                      const bool exclude_empty) {
        std::vector<std::string> parts;
        size_t start = 0;
        size_t end = 0;
        const std::string ss{s};
        while (end != std::string::npos) {
            end = s.find_first_of(delims, start);
            std::string part = ss.substr(start, end - start);
            if (!part.empty() || !exclude_empty) {
                parts.push_back(part);
            }
            start = end + 1;
        }
        return parts;
    }

    std::vector<std::string> StringUtil::split_string(const std::string_view s, const std::string_view delims) {
        return split_string(s, delims, true);
    }

    void StringUtil::ltrim(std::string& s) {
        s.erase(s.begin(), std::ranges::find_if(s.begin(), s.end(), [](const int ch) {
            return !std::isspace(ch);
        }));
    }

    std::string StringUtil::ltrim_copy(std::string s) {
        StringUtil::ltrim(s);
        return s;
    }

    void StringUtil::rtrim(std::string& s) {
        s.erase(std::find_if(s.rbegin(), s.rend(), [](const int ch) {
            return !std::isspace(ch);
        }).base(), s.end());
    }

    std::string StringUtil::rtrim_copy(std::string s) {
        StringUtil::rtrim(s);
        return s;
    }

    void StringUtil::trim(std::string &s) {
        ltrim(s);
        rtrim(s);
    }

    std::string StringUtil::trim_copy(std::string s) {
        StringUtil::trim(s);
        return s;
    }

    bool StringUtil::char_in_string(const char c, const std::string_view s) {
        return s.find(c) != std::string::npos;
    }

    bool StringUtil::string_in_unordered_set(const std::string_view s, const std::unordered_set<std::string>& set) {
        return set.contains(std::string{s});
    }

    bool StringUtil::string_in_vector(const std::string_view s, const std::vector<std::string>& vec) {
        return std::ranges::find(vec.begin(), vec.end(), std::string{s}) != vec.end();
    }

    std::vector<std::string> StringUtil::filter_string_vector(const std::vector<std::string>& vec,
                                                              const std::function<bool(const std::string&)>& predicate) {
        std::vector<std::string> filtered = vec;
        std::erase_if(filtered, std::not_fn(predicate));
        return filtered;
    }

    std::string StringUtil::bool_to_string(const bool b) {
        return b ? "true" : "false";
    }

    // // TODO: want string to be sorted
    std::string StringUtil::unordered_string_set_to_string(const std::unordered_set<std::string>& set) {
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

    std::vector<std::string> StringUtil::split_string(const std::string& str, char delimiter) {
        std::vector<std::string> tokens;
        size_t start = 0;
        size_t end = str.find(delimiter);

        while (end != std::string::npos) {
            tokens.push_back(str.substr(start, end - start));
            start = end + 1;
            end = str.find(delimiter, start);
        }
        tokens.push_back(str.substr(start)); // Add the last token

        return tokens;
    }

    long StringUtil::date_str_to_long(const std::string_view date_str) {
        std::tm tm{};
        if (strptime(std::string{date_str}.c_str(), "%Y-%m-%d", &tm) == nullptr) {
            return -1;
        }
        return std::mktime(&tm);
    }

    std::string StringUtil::long_to_date_str(const long time) {
        if (time == 0L) return "0";
        char buf[11];
        struct tm *tm = localtime(&time);

        if (strftime(buf, sizeof(buf), "%Y-%m-%d", tm) == 0) {
            return "0";
        }
        std::string ds = "\"";
        ds.append(buf);
        ds.append("\"");
        return ds;
    }
}
