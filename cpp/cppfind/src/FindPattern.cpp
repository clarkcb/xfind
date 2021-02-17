#include "FindPattern.h"

namespace cppfind {
    FindPattern::FindPattern(const std::string& pattern) {
        m_pattern = pattern;
        m_regex = std::regex(pattern);
    }

    std::string FindPattern::pattern() const {
        return m_pattern;
    }

    std::regex FindPattern::r() const {
        return m_regex;
    }
}
