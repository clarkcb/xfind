#ifndef CPPFIND_STRINGUTIL_H
#define CPPFIND_STRINGUTIL_H

#include <algorithm>
#include <functional>
#include <iterator>
#include <string>
#include <set>
#include <vector>

namespace cppfind {
    class StringUtil {
    public:
        static std::vector<std::string> split_string(const std::string& s, const std::string& delims);
        static std::vector<std::string> split_string(const std::string& s, const std::string& delims, bool exclude_empty);
        static void ltrim(std::string& s);
        static std::string ltrim_copy(std::string s);
        static void rtrim(std::string& s);
        static std::string rtrim_copy(std::string s);
        static void trim(std::string& s);
        static std::string trim_copy(std::string s);

        static bool char_in_string(char c, const std::string& s);
        static bool string_in_set(const std::string& s, const std::set<std::string>& set);
        static bool string_in_vector(const std::string& s, const std::vector<std::string>& set);

        static std::set<std::string> filter_string_set(const std::set<std::string>& set,
                                                       const std::function<bool(const std::string&)>& predicate);
        static std::vector<std::string> filter_string_vector(const std::vector<std::string>& vec,
                                                             const std::function<bool(const std::string&)>& predicate);

        static std::string bool_to_string(bool b);
        static std::string string_set_to_string(std::set<std::string>& set);
        // static std::string string_vector_to_string(std::vector<std::string>& v);

        static long date_str_to_long(const std::string& date_str);
        static std::string long_to_date_str(long time);

    private:
        // Disallow creating an instance of this object
        StringUtil() = delete;
    };
}

#endif // CPPFIND_STRINGUTIL_H
