#ifndef CPPFIND_REGEXPATTERN_H
#define CPPFIND_REGEXPATTERN_H

#include <regex>
#include <string>

namespace cppfind {
    class RegexPattern {
    public:
        explicit RegexPattern(const std::string& pattern);
        RegexPattern(const std::string& pattern, bool ignore_case, bool multi_line, bool dot_all);
        [[nodiscard]] std::string pattern() const;
        [[nodiscard]] bool ignore_case() const;
        [[nodiscard]] bool multi_line() const;
        [[nodiscard]] bool dot_all() const;
        [[nodiscard]] std::regex regex() const;

        // convert to string
        std::string string();

    private:
        std::string m_pattern;
        bool m_ignore_case;
        bool m_multi_line;
        bool m_dot_all;
        std::regex m_regex;
    };

    struct RegexPatternCmp
    {
        bool operator()(const RegexPattern& lhs, const RegexPattern& rhs) const
        {
            return lhs.pattern() < rhs.pattern();
        }
    };
}

#endif // CPPFIND_REGEXPATTERN_H
