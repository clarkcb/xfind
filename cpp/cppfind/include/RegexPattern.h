#ifndef CPPFIND_REGEXPATTERN_H
#define CPPFIND_REGEXPATTERN_H

#include <regex>
#include <string>

namespace cppfind {
    class RegexPattern {
    public:
        explicit RegexPattern(std::string_view pattern);
        RegexPattern(std::string_view pattern, bool ignore_case, bool multi_line, bool dot_all);
        RegexPattern();
        [[nodiscard]] std::string pattern() const;
        [[nodiscard]] bool ignore_case() const;
        [[nodiscard]] bool multi_line() const;
        [[nodiscard]] bool dot_all() const;
        [[nodiscard]] std::regex regex() const;
        [[nodiscard]] std::string string() const;
        bool operator==(const RegexPattern& other) const;

    private:
        std::string m_pattern;
        bool m_ignore_case;
        bool m_multi_line;
        bool m_dot_all;
        std::regex m_regex;
    };

    struct RegexPatternHash {
        std::size_t operator()(const RegexPattern& r) const noexcept {
            constexpr std::hash<std::string> string_hash;
            return string_hash(r.pattern());
        }
    };
}

#endif // CPPFIND_REGEXPATTERN_H
