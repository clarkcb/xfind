#ifndef CPPFIND_FINDPATTERN_H
#define CPPFIND_FINDPATTERN_H

#include <regex>

namespace cppfind {
    class FindPattern {
    private:
        std::string m_pattern;
        std::regex m_regex;

    public:
        explicit FindPattern(const std::string& pattern);
        std::string pattern() const;
        std::regex r() const;
    };
}

#endif //CPPFIND_FINDPATTERN_H
