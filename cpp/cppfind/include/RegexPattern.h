#ifndef CPPFIND_REGEXPATTERN_H
#define CPPFIND_REGEXPATTERN_H

#include <regex>

namespace cppfind {
    class RegexPattern {
    private:
        std::string m_pattern;
        std::regex m_regex;

    public:
        explicit RegexPattern(const std::string& pattern);
        [[nodiscard]] std::string pattern() const;
        [[nodiscard]] std::regex r() const;
    };
}

#endif //CPPFIND_REGEXPATTERN_H
