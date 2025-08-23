#ifndef CPPFIND_STRINGUTIL_H
#define CPPFIND_STRINGUTIL_H

#include <functional>
#include <string>
#include <unordered_set>
#include <vector>

namespace cppfind {
    class StringUtil {
    public:
        static std::vector<std::string> split_string(std::string_view s, std::string_view delims);
        static std::vector<std::string> split_string(std::string_view s, std::string_view delims, bool exclude_empty);
        static void ltrim(std::string& s);
        static std::string ltrim_copy(std::string s);
        static void rtrim(std::string& s);
        static std::string rtrim_copy(std::string s);
        static void trim(std::string& s);
        static std::string trim_copy(std::string s);

        static bool char_in_string(char c, std::string_view s);
        static bool string_in_unordered_set(std::string_view s, const std::unordered_set<std::string>& set);
        static bool string_in_vector(std::string_view s, const std::vector<std::string>& vec);

        static std::vector<std::string> filter_string_vector(const std::vector<std::string>& vec,
                                                             const std::function<bool(const std::string&)>& predicate);
        static std::string bool_to_string(bool b);
        static std::string unordered_string_set_to_string(const std::unordered_set<std::string>& set);
        static std::vector<std::string> split_string(const std::string& str, char delimiter);
        static long date_str_to_long(std::string_view date_str);
        static std::string long_to_date_str(long time);

    private:
        // Disallow creating an instance of this object
        StringUtil() = delete;
    };
}

#endif // CPPFIND_STRINGUTIL_H
